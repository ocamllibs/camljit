(* Internal assembler for amd64: encodes the assembly programs produced by the
   compiler ([X86_ast.asm_program]) into machine code ([Object.t]),
   without running an external assembler.

   Only the instructions and directives emitted by the amd64 backend are
   supported. The encoding follows the choices of the GNU assembler (shortest
   immediates, accumulator forms, short jumps to local labels when they are
   in range, ...), so that the code can be compared byte for byte with the
   output of [as] (see [tests/encoder]).

   References to local labels of the same section are resolved here. The
   other references (to global symbols, even defined in the same unit, to
   external symbols, and to labels of other sections) produce relocations,
   resolved by [Loader] when the code is loaded. *)

open X86_ast

exception Unsupported of string

let unsupported fmt = Printf.ksprintf (fun msg -> raise (Unsupported msg)) fmt

let () =
  Printexc.register_printer (function
    | Unsupported msg -> Some ("X86_encoder: unsupported " ^ msg)
    | _ -> None)

let reg_number (r : reg64) : int =
  match r with
  | RAX -> 0 | RCX -> 1 | RDX -> 2 | RBX -> 3
  | RSP -> 4 | RBP -> 5 | RSI -> 6 | RDI -> 7
  | R8 -> 8 | R9 -> 9 | R10 -> 10 | R11 -> 11
  | R12 -> 12 | R13 -> 13 | R14 -> 14 | R15 -> 15

let reg8h_number (r : reg8h) : int =
  match r with AH -> 4 | CH -> 5 | DH -> 6 | BH -> 7

let condition_code (c : condition) : int =
  match c with
  | O -> 0 | NO -> 1 | B -> 2 | AE -> 3 | E -> 4 | NE -> 5 | BE -> 6 | A -> 7
  | S -> 8 | NS -> 9 | P -> 10 | NP -> 11 | L -> 12 | GE -> 13 | LE -> 14
  | G -> 15

let float_condition_code (c : float_condition) : int =
  match c with
  | EQf -> 0 | LTf -> 1 | LEf -> 2 | UNORDf -> 3 | NEQf -> 4 | NLTf -> 5
  | NLEf -> 6 | ORDf -> 7

let rounding_code (r : rounding) : int =
  match r with
  | RoundNearest -> 0 | RoundDown -> 1 | RoundUp -> 2 | RoundTruncate -> 3

let fits_int8 (n : int64) : bool = -128L <= n && n <= 127L

let fits_int32 (n : int64) : bool = -0x8000_0000L <= n && n <= 0x7FFF_FFFFL

(* Symbol references are suffixed with [@PLT] (calls through the procedure
   linkage table) or [@GOTPCREL] (the address of the symbol in the global
   offset table). *)
type reference =
  | Direct of string
  | Plt of string
  | Gotpcrel of string

let parse_reference (s : string) : reference =
  let strip suffix =
    if String.ends_with ~suffix s then
      Some (String.sub s 0 (String.length s - String.length suffix))
    else
      None in
  match strip "@PLT" with
  | Some s -> Plt s
  | None ->
     match strip "@GOTPCREL" with
     | Some s -> Gotpcrel s
     | None -> Direct s

(* {1 Instructions} *)

(* A field of an instruction whose value depends on a symbol. *)
type fixup = {
    position : int;
      (** Offset of the field in the instruction. *)
    width : int;
    kind : Object.relocation_kind;
    target : string;
    displacement : int64;
      (** For the relative kinds, the field is the displacement to
          [target + displacement] from the end of the instruction;
          otherwise, the field is [target + displacement]. *)
  }

type encoded = {
    code : string;
    fixups : fixup list;
  }

(* Operand sizes, as inferred by the GNU assembler from the registers or the
   types of the memory operands (see [X86_gas.suf]). *)
type size = Byte | Word | Dword | Qword

let size_of_data_type (t : data_type) : size option =
  match t with
  | BYTE -> Some Byte
  | WORD -> Some Word
  | DWORD | REAL4 -> Some Dword
  | QWORD | REAL8 -> Some Qword
  | NONE | OWORD | NEAR | PROC -> None

let size_of_arg (a : arg) : size option =
  match a with
  | Reg8L _ | Reg8H _ -> Some Byte
  | Reg16 _ -> Some Word
  | Reg32 _ -> Some Dword
  | Reg64 _ -> Some Qword
  | Mem { typ; _ } | Mem64_RIP (typ, _, _) -> size_of_data_type typ
  | Imm _ | Sym _ | Regf _ -> None

(* The size of an operation on [src] and [dst], given by the destination,
   or by the source if the destination does not determine it. *)
let operation_size (src : arg) (dst : arg) : size =
  match size_of_arg dst with
  | Some size -> size
  | None ->
     match size_of_arg src with
     | Some size -> size
     | None -> Qword

(* The operand of the [r/m] field of the ModRM byte. *)
type rm =
  | Rm_reg of int
  | Rm_mem of addr
  | Rm_rip of string * int

let rm_of_arg (a : arg) : rm =
  match a with
  | Reg8L r | Reg16 r | Reg32 r | Reg64 r -> Rm_reg (reg_number r)
  | Reg8H r -> Rm_reg (reg8h_number r)
  | Regf (XMM n) -> Rm_reg n
  | Mem addr -> Rm_mem addr
  | Mem64_RIP (_, s, displ) -> Rm_rip (s, displ)
  | Imm _ | Sym _ | Regf (TOS | ST _) ->
     unsupported "operand is not a register or memory"

let reg_of_arg (a : arg) : int =
  match a with
  | Reg8L r | Reg16 r | Reg32 r | Reg64 r -> reg_number r
  | Reg8H r -> reg8h_number r
  | Regf (XMM n) -> n
  | _ -> unsupported "operand is not a register"

(* The low byte registers %spl, %bpl, %sil and %dil are only accessible with
   a REX prefix (without it, these numbers denote %ah, %ch, %dh, %bh). *)
let needs_rex_for_byte (a : arg) : bool =
  match a with
  | Reg8L (RSP | RBP | RSI | RDI) -> true
  | _ -> false

let is_reg8h (a : arg) : bool =
  match a with Reg8H _ -> true | _ -> false

type immediate =
  | No_immediate
  | Imm8 of int64
  | Imm16 of int64
  | Imm32 of int64
  | Imm64 of int64
  | Imm32_symbol of string * int64
    (** The 32-bit (sign-extended) address of a symbol, plus a constant. *)

(* [instruction ~prefixes ~rex_w ~opcode ~reg ~rm ~imm ~args ()] encodes an
   instruction: legacy [prefixes], a REX prefix if needed, the [opcode]
   bytes, the ModRM byte (and SIB and displacement) for [reg] and [rm] if
   [rm] is given (the register number of [reg] is added to the last opcode
   byte otherwise), and the immediate. [args] are the operands, used to
   check the REX prefix needed by the byte registers. *)
let instruction ?(prefixes = []) ?(rex_w = false) ~(opcode : int list)
      ?(reg = 0) ?rm ?(imm = No_immediate) ?(args = []) () : encoded =
  let b = Buffer.create 16 in
  let fixups = ref [] in
  List.iter (fun p -> Buffer.add_char b (Char.chr p)) prefixes;
  let rex_r = reg >= 8 in
  let rex_x, rex_b =
    match rm with
    | None -> false, reg >= 8
    | Some (Rm_reg n) -> false, n >= 8
    | Some (Rm_rip _) -> false, false
    | Some (Rm_mem { idx; scale; base; _ }) ->
       begin match scale, base with
       | 0, _ -> false, false
       | 1, None -> false, reg_number idx >= 8
       | _, None -> reg_number idx >= 8, false
       | _, Some base -> reg_number idx >= 8, reg_number base >= 8
       end in
  let rex_r = rm <> None && rex_r in
  let force_rex = List.exists needs_rex_for_byte args in
  if rex_w || rex_r || rex_x || rex_b || force_rex then
    begin
      if List.exists is_reg8h args then
        unsupported "%%ah, %%bh, %%ch or %%dh with a REX prefix";
      Buffer.add_char b (Char.chr
        (0x40 lor (if rex_w then 8 else 0) lor (if rex_r then 4 else 0)
         lor (if rex_x then 2 else 0) lor (if rex_b then 1 else 0)))
    end;
  let opcode =
    match rm with
    | None ->
       (* The register is in the low bits of the last opcode byte. *)
       let rec add_reg = function
         | [] -> []
         | [last] -> [last + (reg land 7)]
         | o :: os -> o :: add_reg os in
       add_reg opcode
    | Some _ -> opcode in
  List.iter (fun o -> Buffer.add_char b (Char.chr o)) opcode;
  let add_int32 (n : int64) =
    Buffer.add_int32_le b (Int64.to_int32 n) in
  let add_symbol_field ~kind ~target ~displacement =
    fixups :=
      { position = Buffer.length b; width = 4; kind; target; displacement }
      :: !fixups;
    add_int32 0L in
  let modrm md reg rm =
    Buffer.add_char b (Char.chr ((md lsl 6) lor ((reg land 7) lsl 3) lor (rm land 7))) in
  begin match rm with
  | None -> ()
  | Some (Rm_reg n) -> modrm 3 reg n
  | Some (Rm_rip (s, displ)) ->
     modrm 0 reg 5;
     let (kind : Object.relocation_kind), target =
       match parse_reference s with
       | Direct s -> Relative32, s
       | Gotpcrel s -> Got_relative32, s
       | Plt s -> Branch32, s in
     add_symbol_field ~kind ~target ~displacement:(Int64.of_int displ)
  | Some (Rm_mem { idx; scale; base; sym; displ; _ }) ->
     let displacement md =
       match sym with
       | Some s ->
          add_symbol_field ~kind:Absolute32_signed ~target:s
            ~displacement:(Int64.of_int displ)
       | None ->
          if md = 1 then Buffer.add_int8 b displ
          else add_int32 (Int64.of_int displ) in
     (* The mode of the displacement: none (0), 8 bits (1) or 32 bits (2).
        A base register whose low bits are 5 (%rbp, %r13) needs a
        displacement. *)
     let mode base_number =
       if sym = None && displ = 0 && base_number land 7 <> 5 then 0
       else if sym = None && fits_int8 (Int64.of_int displ) then 1
       else 2 in
     let scale_bits =
       match scale with
       | 1 -> 0 | 2 -> 1 | 4 -> 2 | 8 -> 3
       | _ -> unsupported "scale %d" scale in
     begin match scale, base with
     | 0, _ ->
        (* Absolute address: SIB without base nor index. *)
        modrm 0 reg 4;
        Buffer.add_char b '\x25';
        displacement 2
     | 1, None ->
        (* [idx] is the base register. *)
        let n = reg_number idx in
        let md = mode n in
        if n land 7 = 4 then
          begin
            modrm md reg 4;
            Buffer.add_char b (Char.chr (0x20 lor (n land 7)))
          end
        else
          modrm md reg n;
        if md <> 0 then displacement md
     | _, None ->
        (* Index without base: 32-bit displacement. *)
        if idx = RSP then unsupported "%%rsp as index";
        modrm 0 reg 4;
        Buffer.add_char b (Char.chr
          ((scale_bits lsl 6) lor ((reg_number idx land 7) lsl 3) lor 5));
        displacement 2
     | _, Some base ->
        if idx = RSP then unsupported "%%rsp as index";
        let n = reg_number base in
        let md = mode n in
        modrm md reg 4;
        Buffer.add_char b (Char.chr
          ((scale_bits lsl 6) lor ((reg_number idx land 7) lsl 3) lor (n land 7)));
        if md <> 0 then displacement md
     end
  end;
  begin match imm with
  | No_immediate -> ()
  | Imm8 n -> Buffer.add_int8 b (Int64.to_int n)
  | Imm16 n -> Buffer.add_int16_le b (Int64.to_int n)
  | Imm32 n -> add_int32 n
  | Imm64 n -> Buffer.add_int64_le b n
  | Imm32_symbol (s, n) ->
     add_symbol_field ~kind:Absolute32_signed ~target:s ~displacement:n
  end;
  { code = Buffer.contents b; fixups = !fixups }

let w_of_size (size : size) : bool = size = Qword

let prefixes_of_size (size : size) : int list =
  if size = Word then [0x66] else []

let is_accumulator (a : arg) : bool =
  match a with
  | Reg8L RAX | Reg16 RAX | Reg32 RAX | Reg64 RAX -> true
  | _ -> false

let is_register (a : arg) : bool =
  match a with
  | Reg8L _ | Reg8H _ | Reg16 _ | Reg32 _ | Reg64 _ | Regf _ -> true
  | _ -> false

let immediate_of_size (size : size) (n : int64) : immediate =
  match size with
  | Byte -> Imm8 n
  | Word -> Imm16 n
  | Dword | Qword -> Imm32 n

(* Arithmetic instructions with the opcode extension [op]: add (0), or (1),
   and (4), sub (5), xor (6), cmp (7). *)
let arithmetic (op : int) (src : arg) (dst : arg) : encoded =
  let size = operation_size src dst in
  let prefixes = prefixes_of_size size in
  let rex_w = w_of_size size in
  let args = [src; dst] in
  match src with
  | Imm n ->
     if size = Byte then
       if is_accumulator dst then
         instruction ~prefixes ~opcode:[op * 8 + 4] ~imm:(Imm8 n) ~args ()
       else
         instruction ~prefixes ~opcode:[0x80] ~reg:op ~rm:(rm_of_arg dst)
           ~imm:(Imm8 n) ~args ()
     else if fits_int8 n then
       instruction ~prefixes ~rex_w ~opcode:[0x83] ~reg:op ~rm:(rm_of_arg dst)
         ~imm:(Imm8 n) ~args ()
     else if is_accumulator dst then
       instruction ~prefixes ~rex_w ~opcode:[op * 8 + 5]
         ~imm:(immediate_of_size size n) ~args ()
     else
       instruction ~prefixes ~rex_w ~opcode:[0x81] ~reg:op ~rm:(rm_of_arg dst)
         ~imm:(immediate_of_size size n) ~args ()
  | Sym s ->
     instruction ~prefixes ~rex_w ~opcode:[0x81] ~reg:op ~rm:(rm_of_arg dst)
       ~imm:(Imm32_symbol (s, 0L)) ~args ()
  | _ when is_register src ->
     let opcode = op * 8 + (if size = Byte then 0 else 1) in
     instruction ~prefixes ~rex_w ~opcode:[opcode] ~reg:(reg_of_arg src)
       ~rm:(rm_of_arg dst) ~args ()
  | _ ->
     let opcode = op * 8 + (if size = Byte then 2 else 3) in
     instruction ~prefixes ~rex_w ~opcode:[opcode] ~reg:(reg_of_arg dst)
       ~rm:(rm_of_arg src) ~args ()

(* Unary instructions [opcode /op] on [dst] ([opcode - 1] for bytes). *)
let unary (opcode : int) (op : int) (dst : arg) : encoded =
  let size = operation_size dst dst in
  let opcode = if size = Byte then opcode - 1 else opcode in
  instruction ~prefixes:(prefixes_of_size size) ~rex_w:(w_of_size size)
    ~opcode:[opcode] ~reg:op ~rm:(rm_of_arg dst) ~args:[dst] ()

(* Shifts: sal (4), shr (5), sar (7). *)
let shift (op : int) (count : arg) (dst : arg) : encoded =
  let size = operation_size dst dst in
  let prefixes = prefixes_of_size size in
  let rex_w = w_of_size size in
  let byte = if size = Byte then 1 else 0 in
  let args = [dst] in
  match count with
  | Imm 1L ->
     instruction ~prefixes ~rex_w ~opcode:[0xD1 - byte] ~reg:op
       ~rm:(rm_of_arg dst) ~args ()
  | Imm n ->
     instruction ~prefixes ~rex_w ~opcode:[0xC1 - byte] ~reg:op
       ~rm:(rm_of_arg dst) ~imm:(Imm8 n) ~args ()
  | Reg8L RCX ->
     instruction ~prefixes ~rex_w ~opcode:[0xD3 - byte] ~reg:op
       ~rm:(rm_of_arg dst) ~args ()
  | _ -> unsupported "shift count"

(* SSE instructions [prefix 0F opcode] with [dst] in the reg field. *)
let sse ?(rex_w = false) ?imm (prefix : int) (opcode : int list) (src : arg)
      (dst : arg) : encoded =
  instruction ~prefixes:[prefix] ~rex_w ~opcode:(0x0F :: opcode)
    ~reg:(reg_of_arg dst) ~rm:(rm_of_arg src) ?imm ~args:[src; dst] ()

let is_xmm (a : arg) : bool =
  match a with Regf (XMM _) -> true | _ -> false

let mov (src : arg) (dst : arg) : encoded =
  let size = operation_size src dst in
  let prefixes = prefixes_of_size size in
  let rex_w = w_of_size size in
  let args = [src; dst] in
  match src, dst with
  | Imm n, Reg64 r when not (fits_int32 n) ->
     instruction ~rex_w:true ~opcode:[0xB8] ~reg:(reg_number r) ~imm:(Imm64 n)
       ~args ()
  | Imm n, (Reg32 _ | Reg16 _ | Reg8L _ | Reg8H _) ->
     let opcode = if size = Byte then 0xB0 else 0xB8 in
     instruction ~prefixes ~opcode:[opcode] ~reg:(reg_of_arg dst)
       ~imm:(immediate_of_size size n) ~args ()
  | Imm n, _ ->
     let opcode = if size = Byte then 0xC6 else 0xC7 in
     instruction ~prefixes ~rex_w ~opcode:[opcode] ~reg:0 ~rm:(rm_of_arg dst)
       ~imm:(immediate_of_size size n) ~args ()
  | Sym s, _ ->
     instruction ~prefixes ~rex_w ~opcode:[0xC7] ~reg:0 ~rm:(rm_of_arg dst)
       ~imm:(Imm32_symbol (s, 0L)) ~args ()
  | _, _ when is_register src ->
     let opcode = if size = Byte then 0x88 else 0x89 in
     instruction ~prefixes ~rex_w ~opcode:[opcode] ~reg:(reg_of_arg src)
       ~rm:(rm_of_arg dst) ~args ()
  | _, _ ->
     let opcode = if size = Byte then 0x8A else 0x8B in
     instruction ~prefixes ~rex_w ~opcode:[opcode] ~reg:(reg_of_arg dst)
       ~rm:(rm_of_arg src) ~args ()

(* Instructions whose encoding does not depend on label positions (all but
   the jumps to local labels, see [layout]). *)
(* [bsf] and [bsr]: the destination is a register. *)
let bit_scan (opcode : int) (src : arg) (dst : arg) : encoded =
  let size = operation_size dst dst in
  instruction ~prefixes:(prefixes_of_size size) ~rex_w:(w_of_size size)
    ~opcode:[0x0F; opcode] ~reg:(reg_of_arg dst) ~rm:(rm_of_arg src)
    ~args:[src; dst] ()

(* [xadd], with the given prefixes (lock): the source is a register. *)
let exchange_add (lock : int list) (src : arg) (dst : arg) : encoded =
  let size = operation_size src dst in
  instruction ~prefixes:(lock @ prefixes_of_size size) ~rex_w:(w_of_size size)
    ~opcode:[0x0F; if size = Byte then 0xC0 else 0xC1] ~reg:(reg_of_arg src)
    ~rm:(rm_of_arg dst) ~args:[src; dst] ()

(* The instructions that only exist in some versions of the compiler are
   recognized by [X86_extra] (selected by dune according to the version of
   OCaml), so that this file is compiled without preprocessor: the
   [X86_ast.instruction] constructors of OCaml >= 5.6 cannot be named in the
   patterns below. *)
let encode_extra (i : X86_extra.t) : encoded =
  match i with
  (* The instructions of OCaml >= 5.6: [bsr] and [bsf] (with [bts], to set
     the top bit) count the leading and trailing zeros, [xadd] (with the lock
     prefix with several domains) is [Atomic.fetch_and_add]. *)
  | Bsf (src, dst) -> bit_scan 0xBC src dst
  | Bsr (src, dst) -> bit_scan 0xBD src dst
  | Bts (Imm n, dst) ->
     let size = operation_size dst dst in
     instruction ~prefixes:(prefixes_of_size size) ~rex_w:(w_of_size size)
       ~opcode:[0x0F; 0xBA] ~reg:5 ~rm:(rm_of_arg dst) ~imm:(Imm8 n)
       ~args:[dst] ()
  | Bts (src, dst) ->
     let size = operation_size src dst in
     instruction ~prefixes:(prefixes_of_size size) ~rex_w:(w_of_size size)
       ~opcode:[0x0F; 0xAB] ~reg:(reg_of_arg src) ~rm:(rm_of_arg dst)
       ~args:[src; dst] ()
  | Xadd (src, dst) -> exchange_add [] src dst
  | Lock_xadd (src, dst) -> exchange_add [0xF0] src dst

let encode_instruction (i : instruction) : encoded =
  match X86_extra.classify i with
  | Some extra -> encode_extra extra
  | None ->
  (* The last case is unused before OCaml 5.6, where all the constructors
     are listed. *)
  begin[@warning "-11"] match i with
  | ADD (src, dst) -> arithmetic 0 src dst
  | OR (src, dst) -> arithmetic 1 src dst
  | AND (src, dst) -> arithmetic 4 src dst
  | SUB (src, dst) -> arithmetic 5 src dst
  | XOR (src, dst) -> arithmetic 6 src dst
  | CMP (src, dst) -> arithmetic 7 src dst
  | TEST (src, dst) ->
     let size = operation_size src dst in
     let prefixes = prefixes_of_size size in
     let rex_w = w_of_size size in
     let args = [src; dst] in
     begin match src with
     | Imm n when is_accumulator dst ->
        instruction ~prefixes ~rex_w ~opcode:[if size = Byte then 0xA8 else 0xA9]
          ~imm:(immediate_of_size size n) ~args ()
     | Imm n ->
        instruction ~prefixes ~rex_w ~opcode:[if size = Byte then 0xF6 else 0xF7]
          ~reg:0 ~rm:(rm_of_arg dst) ~imm:(immediate_of_size size n) ~args ()
     | _ ->
        let opcode = if size = Byte then 0x84 else 0x85 in
        if is_register src then
          instruction ~prefixes ~rex_w ~opcode:[opcode] ~reg:(reg_of_arg src)
            ~rm:(rm_of_arg dst) ~args ()
        else
          instruction ~prefixes ~rex_w ~opcode:[opcode] ~reg:(reg_of_arg dst)
            ~rm:(rm_of_arg src) ~args ()
     end
  | MOV (src, dst) -> mov src dst
  | LEA (src, dst) ->
     let size = operation_size dst dst in
     instruction ~prefixes:(prefixes_of_size size) ~rex_w:(w_of_size size)
       ~opcode:[0x8D] ~reg:(reg_of_arg dst) ~rm:(rm_of_arg src) ~args:[dst] ()
  | MOVZX (src, dst) | MOVSX (src, dst) ->
     let size = operation_size dst dst in
     let base = match i with MOVZX _ -> 0xB6 | _ -> 0xBE in
     let opcode = if size_of_arg src = Some Word then base + 1 else base in
     instruction ~prefixes:(prefixes_of_size size) ~rex_w:(w_of_size size)
       ~opcode:[0x0F; opcode] ~reg:(reg_of_arg dst) ~rm:(rm_of_arg src)
       ~args:[src; dst] ()
  | MOVSXD (src, dst) ->
     instruction ~rex_w:true ~opcode:[0x63] ~reg:(reg_of_arg dst)
       ~rm:(rm_of_arg src) ~args:[src; dst] ()
  | CMOV (c, src, dst) ->
     let size = operation_size dst dst in
     instruction ~prefixes:(prefixes_of_size size) ~rex_w:(w_of_size size)
       ~opcode:[0x0F; 0x40 + condition_code c] ~reg:(reg_of_arg dst)
       ~rm:(rm_of_arg src) ~args:[src; dst] ()
  | SET (c, dst) ->
     instruction ~opcode:[0x0F; 0x90 + condition_code c] ~reg:0
       ~rm:(rm_of_arg dst) ~args:[dst] ()
  | CALL (Sym s) ->
     let target = match parse_reference s with Direct s | Plt s | Gotpcrel s -> s in
     let e = instruction ~opcode:[0xE8] ~imm:(Imm32 0L) () in
     { e with fixups = [{ position = 1; width = 4; kind = Branch32; target;
                          displacement = 0L }] }
  | CALL dst ->
     instruction ~opcode:[0xFF] ~reg:2 ~rm:(rm_of_arg dst) ~args:[dst] ()
  | JMP (Sym s) ->
     let target = match parse_reference s with Direct s | Plt s | Gotpcrel s -> s in
     let e = instruction ~opcode:[0xE9] ~imm:(Imm32 0L) () in
     { e with fixups = [{ position = 1; width = 4; kind = Branch32; target;
                          displacement = 0L }] }
  | JMP dst ->
     instruction ~opcode:[0xFF] ~reg:4 ~rm:(rm_of_arg dst) ~args:[dst] ()
  | J (c, Sym s) ->
     let target = match parse_reference s with Direct s | Plt s | Gotpcrel s -> s in
     let e = instruction ~opcode:[0x0F; 0x80 + condition_code c] ~imm:(Imm32 0L) () in
     { e with fixups = [{ position = 2; width = 4; kind = Branch32; target;
                          displacement = 0L }] }
  | J _ -> unsupported "indirect conditional jump"
  | INC dst -> unary 0xFF 0 dst
  | DEC dst -> unary 0xFF 1 dst
  | NEG dst -> unary 0xF7 3 dst
  | IDIV dst -> unary 0xF7 7 dst
  | IMUL (dst, None) -> unary 0xF7 5 dst
  | IMUL (Imm n, Some dst) ->
     let size = operation_size dst dst in
     let imm, opcode =
       if fits_int8 n then Imm8 n, 0x6B else immediate_of_size size n, 0x69 in
     instruction ~prefixes:(prefixes_of_size size) ~rex_w:(w_of_size size)
       ~opcode:[opcode] ~reg:(reg_of_arg dst) ~rm:(rm_of_arg dst) ~imm
       ~args:[dst] ()
  | IMUL (src, Some dst) ->
     let size = operation_size dst dst in
     instruction ~prefixes:(prefixes_of_size size) ~rex_w:(w_of_size size)
       ~opcode:[0x0F; 0xAF] ~reg:(reg_of_arg dst) ~rm:(rm_of_arg src)
       ~args:[src; dst] ()
  | SAL (count, dst) -> shift 4 count dst
  | SHR (count, dst) -> shift 5 count dst
  | SAR (count, dst) -> shift 7 count dst
  | PUSH (Reg64 r) -> instruction ~opcode:[0x50] ~reg:(reg_number r) ()
  | PUSH (Imm n) when fits_int8 n -> instruction ~opcode:[0x6A] ~imm:(Imm8 n) ()
  | PUSH (Imm n) -> instruction ~opcode:[0x68] ~imm:(Imm32 n) ()
  | PUSH dst -> instruction ~opcode:[0xFF] ~reg:6 ~rm:(rm_of_arg dst) ~args:[dst] ()
  | POP (Reg64 r) -> instruction ~opcode:[0x58] ~reg:(reg_number r) ()
  | POP dst -> instruction ~opcode:[0x8F] ~reg:0 ~rm:(rm_of_arg dst) ~args:[dst] ()
  | CQO -> instruction ~rex_w:true ~opcode:[0x99] ()
  | CDQ -> instruction ~opcode:[0x99] ()
  | RET -> instruction ~opcode:[0xC3] ()
  | NOP -> instruction ~opcode:[0x90] ()
  | HLT -> instruction ~opcode:[0xF4] ()
  | LEAVE -> instruction ~opcode:[0xC9] ()
  | XCHG (a, b) ->
     let size = operation_size a b in
     let prefixes = prefixes_of_size size in
     let rex_w = w_of_size size in
     begin match a, b with
     | (Reg64 _ | Reg32 _), (Reg64 _ | Reg32 _) when is_accumulator a ->
        instruction ~prefixes ~rex_w ~opcode:[0x90] ~reg:(reg_of_arg b) ()
     | (Reg64 _ | Reg32 _), (Reg64 _ | Reg32 _) when is_accumulator b ->
        instruction ~prefixes ~rex_w ~opcode:[0x90] ~reg:(reg_of_arg a) ()
     | _ when is_register a ->
        instruction ~prefixes ~rex_w ~opcode:[if size = Byte then 0x86 else 0x87]
          ~reg:(reg_of_arg a) ~rm:(rm_of_arg b) ~args:[a; b] ()
     | _ ->
        instruction ~prefixes ~rex_w ~opcode:[if size = Byte then 0x86 else 0x87]
          ~reg:(reg_of_arg b) ~rm:(rm_of_arg a) ~args:[a; b] ()
     end
  | BSWAP dst ->
     let size = operation_size dst dst in
     instruction ~rex_w:(w_of_size size) ~opcode:[0x0F; 0xC8]
       ~reg:(reg_of_arg dst) ()
  | ADDSD (src, dst) -> sse 0xF2 [0x58] src dst
  | SUBSD (src, dst) -> sse 0xF2 [0x5C] src dst
  | MULSD (src, dst) -> sse 0xF2 [0x59] src dst
  | DIVSD (src, dst) -> sse 0xF2 [0x5E] src dst
  | SQRTSD (src, dst) -> sse 0xF2 [0x51] src dst
  | ANDPD (src, dst) -> sse 0x66 [0x54] src dst
  | XORPD (src, dst) -> sse 0x66 [0x57] src dst
  | UCOMISD (src, dst) -> sse 0x66 [0x2E] src dst
  | COMISD (src, dst) -> sse 0x66 [0x2F] src dst
  | CVTSD2SS (src, dst) -> sse 0xF2 [0x5A] src dst
  | CVTSS2SD (src, dst) -> sse 0xF3 [0x5A] src dst
  | CMPSD (c, src, dst) ->
     sse 0xF2 [0xC2] src dst ~imm:(Imm8 (Int64.of_int (float_condition_code c)))
  | ROUNDSD (r, src, dst) ->
     sse 0x66 [0x3A; 0x0B] src dst
       ~imm:(Imm8 (Int64.of_int (rounding_code r)))
  | CVTSI2SD (src, dst) ->
     sse 0xF2 [0x2A] src dst ~rex_w:(size_of_arg src = Some Qword)
  | CVTTSD2SI (src, dst) ->
     sse 0xF2 [0x2C] src dst ~rex_w:(size_of_arg dst = Some Qword)
  | CVTSD2SI (src, dst) ->
     sse 0xF2 [0x2D] src dst ~rex_w:(size_of_arg dst = Some Qword)
  | MOVSD (src, dst) | MOVSS (src, dst) | MOVAPD (src, dst) | MOVLPD (src, dst) ->
     let prefix, load, store =
       match i with
       | MOVSD _ -> 0xF2, 0x10, 0x11
       | MOVSS _ -> 0xF3, 0x10, 0x11
       | MOVAPD _ -> 0x66, 0x28, 0x29
       | _ -> 0x66, 0x12, 0x13 in
     if is_xmm dst then sse prefix [load] src dst
     else sse prefix [store] dst src
  | MOVD (src, dst) ->
     if is_xmm dst then
       sse 0x66 [0x6E] src dst ~rex_w:(size_of_arg src = Some Qword)
     else
       sse 0x66 [0x7E] dst src ~rex_w:(size_of_arg dst = Some Qword)
  | FABS | FADD _ | FADDP _ | FCHS | FCOMP _ | FCOMPP | FCOS | FDIV _
  | FDIVP _ | FDIVR _ | FDIVRP _ | FILD _ | FISTP _ | FLD _ | FLD1 | FLDCW _
  | FLDLG2 | FLDLN2 | FLDZ | FMUL _ | FMULP _ | FNSTCW _ | FNSTSW _ | FPATAN
  | FPTAN | FSIN | FSQRT | FSTP _ | FSUB _ | FSUBP _ | FSUBR _ | FSUBRP _
  | FXCH _ | FYL2X ->
     unsupported "x87 instruction"
  | _ -> unsupported "instruction"
  end

(* {1 Sections and layout} *)

type item =
  | Code of encoded
  | Jump of { condition : condition option; label : string;
              mutable long : bool }
    (** A jump to a local label of the same section, encoded in the short
        form (8-bit displacement) if it is in range. *)
  | Data of int * constant
  | Raw of string
  | Zeros of int
  | Alignment of int
  | Label of string

type section_items = {
    section_name : string;
    mutable items : item list;  (** In reverse order. *)
  }

let short_jump_size = 2

let long_jump_size (condition : condition option) : int =
  match condition with None -> 5 | Some _ -> 6

let is_text_section (name : string) : bool =
  name = ".text" || String.starts_with ~prefix:".text." name
  || String.starts_with ~prefix:"__TEXT,__text" name

let section_name (names : string list) : string = String.concat "," names

(* Multi-byte no-operations, as used by the GNU assembler to pad code. *)
let nops = [|
    "";
    "\x90";
    "\x66\x90";
    "\x0f\x1f\x00";
    "\x0f\x1f\x40\x00";
    "\x0f\x1f\x44\x00\x00";
    "\x66\x0f\x1f\x44\x00\x00";
    "\x0f\x1f\x80\x00\x00\x00\x00";
    "\x0f\x1f\x84\x00\x00\x00\x00\x00";
    "\x66\x0f\x1f\x84\x00\x00\x00\x00\x00";
    "\x66\x2e\x0f\x1f\x84\x00\x00\x00\x00\x00";
    "\x66\x66\x2e\x0f\x1f\x84\x00\x00\x00\x00\x00";
  |]

type padding_order = Longest_first | Remainder_first

(* The order of the no-operations that pad code. Any order is correct, but
   the GNU assembler changed it between versions: binutils 2.40 and 2.44 emit
   the longest no-operations first, 2.46 emits the remainder first.
   [tests/encoder], which compares the machine code byte for byte, sets
   it to the order of the assembler it compares with. *)
let padding_order = ref Longest_first

let padding ~(executable : bool) (n : int) : string =
  if not executable then String.make n '\000'
  else
    let longest = Array.length nops - 1 in
    let b = Buffer.create n in
    let remainder () = Buffer.add_string b nops.(n mod longest) in
    if !padding_order = Remainder_first then remainder ();
    for _ = 1 to n / longest do
      Buffer.add_string b nops.(longest)
    done;
    if !padding_order = Longest_first then remainder ();
    Buffer.contents b

let align_up (n : int) (alignment : int) : int =
  (n + alignment - 1) / alignment * alignment

(* The size of each item, given the offset where it starts. *)
let item_size (offset : int) (item : item) : int =
  match item with
  | Code e -> String.length e.code
  | Jump { condition; long; _ } ->
     if long then long_jump_size condition else short_jump_size
  | Data (width, _) -> width
  | Raw s -> String.length s
  | Zeros n -> n
  | Alignment n -> align_up offset n - offset
  | Label _ -> 0

(* Computes the offsets of the labels, choosing the short form for the jumps
   whose label is in range: all jumps start short, and those out of range
   become long until no jump changes (the sizes only grow, so this
   terminates). *)
let layout (items : item array) : (string, int) Hashtbl.t =
  let labels = Hashtbl.create 64 in
  let rec iterate () =
    Hashtbl.reset labels;
    let offsets = Array.make (Array.length items) 0 in
    let offset = ref 0 in
    Array.iteri (fun i item ->
      offsets.(i) <- !offset;
      (match item with Label l -> Hashtbl.replace labels l !offset | _ -> ());
      offset := !offset + item_size !offset item) items;
    let changed = ref false in
    Array.iteri (fun i item ->
      match item with
      | Jump ({ long = false; label; _ } as jump) ->
         let target = Hashtbl.find labels label in
         let displacement = target - (offsets.(i) + short_jump_size) in
         if not (fits_int8 (Int64.of_int displacement)) then
           begin
             jump.long <- true;
             changed := true
           end
      | _ -> ()) items;
    if !changed then iterate () in
  iterate ();
  labels

(* {1 Constants} *)

(* A constant expression, as a sum of a numeric constant and of labels with
   signs ([ConstThis] is the label ["."]). *)
let rec terms (sign : int) (c : constant) (acc : int64 * (int * string) list)
    : int64 * (int * string) list =
  let n, labels = acc in
  match c with
  | Const k -> Int64.add n (Int64.mul (Int64.of_int sign) k), labels
  | ConstThis -> n, (sign, ".") :: labels
  | ConstLabel l -> n, (sign, l) :: labels
  | ConstAdd (c1, c2) -> terms sign c2 (terms sign c1 acc)
  | ConstSub (c1, c2) -> terms (- sign) c2 (terms sign c1 acc)

(* {1 Programs} *)

type program_state = {
    sections : (string, section_items) Hashtbl.t;
    mutable section_order : string list;  (** In reverse order. *)
    mutable current : section_items;
    globals : (string, unit) Hashtbl.t;
    defined : (string, string) Hashtbl.t;
      (** The section of each label. *)
    aliases : (string, constant) Hashtbl.t;
      (** Symbols defined with [.set]. *)
  }

let switch_section (state : program_state) (name : string) : unit =
  match Hashtbl.find_opt state.sections name with
  | Some section -> state.current <- section
  | None ->
     let section = { section_name = name; items = [] } in
     Hashtbl.add state.sections name section;
     state.section_order <- name :: state.section_order;
     state.current <- section

let add_item (state : program_state) (item : item) : unit =
  state.current.items <- item :: state.current.items

let jump_target (a : arg) : string option =
  match a with
  | Sym s ->
     begin match parse_reference s with
     | Direct s -> Some s
     | Plt _ | Gotpcrel _ -> None
     end
  | _ -> None

(* First pass: the items of each section, the labels and their sections, and
   the global symbols. *)
let collect (program : asm_program) : program_state =
  let text = { section_name = ".text"; items = [] } in
  let state = {
      sections = Hashtbl.create 8; section_order = [".text"]; current = text;
      globals = Hashtbl.create 16; defined = Hashtbl.create 64;
      aliases = Hashtbl.create 4 } in
  Hashtbl.add state.sections ".text" text;
  let jump condition target i =
    match jump_target target with
    | Some label ->
       (* Whether the label is local to the section is only known once all
          the labels are defined: see [resolve_jumps]. *)
       add_item state (Jump { condition; label; long = false })
    | None -> add_item state (Code (encode_instruction i)) in
  List.iter (fun (line : asm_line) ->
    match line with
    | Ins (J (c, (Sym _ as target)) as i) -> jump (Some c) target i
    | Ins (JMP (Sym _ as target) as i) -> jump None target i
    | Ins i -> add_item state (Code (encode_instruction i))
    | Align (_, n) -> add_item state (Alignment n)
    | Byte c -> add_item state (Data (1, c))
    | Word c -> add_item state (Data (2, c))
    | Long c -> add_item state (Data (4, c))
    | Quad c -> add_item state (Data (8, c))
    | Bytes s -> add_item state (Raw s)
    | Space n -> add_item state (Zeros n)
    | NewLabel (l, _) ->
       Hashtbl.replace state.defined l state.current.section_name;
       add_item state (Label l)
    | Global s -> Hashtbl.replace state.globals s ()
    | Section (names, _, _) -> switch_section state (section_name names)
    | Set (s, c) -> Hashtbl.replace state.aliases s c
    | Comment _ | External _ | Mode386 | Model _ | Cfi_adjust_cfa_offset _
    | Cfi_endproc | Cfi_startproc | Cfi_remember_state | Cfi_restore_state
    | Cfi_def_cfa_register _ | Cfi_def_cfa_offset _ | File _
    | Indirect_symbol _ | Loc _ | Private_extern _ | Size _ | Type _ ->
       ()) program;
  state

(* Symbols defined with [.set] are replaced by their definition. On macOS,
   the frame tables use [.set L$set$N, label - . + offset] immediately
   followed by [.long L$set$N]: "." has the same position in the definition
   and in the use. *)
let rec expand_aliases (state : program_state) (c : constant) : constant =
  match c with
  | ConstLabel l when Hashtbl.mem state.aliases l ->
     expand_aliases state (Hashtbl.find state.aliases l)
  | ConstAdd (c1, c2) ->
     ConstAdd (expand_aliases state c1, expand_aliases state c2)
  | ConstSub (c1, c2) ->
     ConstSub (expand_aliases state c1, expand_aliases state c2)
  | Const _ | ConstThis | ConstLabel _ -> c

(* A label is local to a section if it is defined in this section and is
   not global (references to global symbols are relocated, as the GNU
   assembler does for ELF, since they may be defined elsewhere). *)
let is_local (state : program_state) (section : string) (label : string) : bool =
  Hashtbl.find_opt state.defined label = Some section
  && not (Hashtbl.mem state.globals label)

(* Jumps to labels that are not local to their section are long jumps with
   a relocation. *)
let resolve_jumps (state : program_state) (section : section_items) : item array =
  Array.of_list (List.rev_map (fun item ->
    match item with
    | Jump { condition; label; _ }
      when not (is_local state section.section_name label) ->
       Code (encode_instruction
         (match condition with
          | Some c -> J (c, Sym label)
          | None -> JMP (Sym label)))
    | _ -> item) section.items)

let encode_section (state : program_state) (index : int)
    (section : section_items) : Object.section * (string * Object.symbol) list =
  let name = section.section_name in
  let executable = is_text_section name in
  let items = resolve_jumps state section in
  let labels = layout items in
  let b = Buffer.create 1024 in
  let relocations = ref [] in
  let alignment = ref 1 in
  let local label = is_local state name label in
  let add_relocation ~offset ~kind ~target ~addend =
    relocations :=
      { Object.offset; kind; target; addend } :: !relocations in
  (* Fields resolved in the section, written once the section is complete. *)
  let patches = ref [] in
  let patch_int ~offset ~width (n : int64) =
    patches := (offset, width, n) :: !patches in
  let add_int ~width (n : int64) =
    match width with
    | 1 -> Buffer.add_int8 b (Int64.to_int n)
    | 2 -> Buffer.add_int16_le b (Int64.to_int n)
    | 4 -> Buffer.add_int32_le b (Int64.to_int32 n)
    | _ -> Buffer.add_int64_le b n in
  Array.iter (fun item ->
    let offset = Buffer.length b in
    match item with
    | Label _ -> ()
    | Code { code; fixups } ->
       Buffer.add_string b code;
       let instruction_end = offset + String.length code in
       List.iter (fun { position; width; kind; target; displacement } ->
         let field = offset + position in
         match kind with
         | Relative32 | Branch32 | Got_relative32 ->
            (* The field is [target + displacement - instruction_end], that
               is [target + addend - field]. *)
            let addend =
              Int64.sub displacement (Int64.of_int (instruction_end - field)) in
            if kind <> Got_relative32 && local target then
              patch_int ~offset:field ~width
                (Int64.add addend
                   (Int64.of_int (Hashtbl.find labels target - field)))
            else
              add_relocation ~offset:field ~kind ~target ~addend
         | Absolute64 | Absolute32_signed ->
            add_relocation ~offset:field ~kind ~target ~addend:displacement
         | Arm64_branch26 | Arm64_page21 | Arm64_got_page21
         | Arm64_got_pageoff12 | Arm64_pageoff12 _ ->
            (* The fixups of this encoder are amd64 relocations. *)
            invalid_arg "X86_encoder: AArch64 relocation")
         fixups
    | Jump { condition; label; long } ->
       let target = Hashtbl.find labels label in
       let size = if long then long_jump_size condition else short_jump_size in
       let displacement = Int64.of_int (target - (offset + size)) in
       begin match condition, long with
       | None, false -> Buffer.add_char b '\xEB'; add_int ~width:1 displacement
       | Some c, false ->
          Buffer.add_char b (Char.chr (0x70 + condition_code c));
          add_int ~width:1 displacement
       | None, true -> Buffer.add_char b '\xE9'; add_int ~width:4 displacement
       | Some c, true ->
          Buffer.add_char b '\x0F';
          Buffer.add_char b (Char.chr (0x80 + condition_code c));
          add_int ~width:4 displacement
       end
    | Data (width, c) ->
       let n, labels_terms = terms 1 (expand_aliases state c) (0L, []) in
       (* The positions of the local labels of this section and of "." are
          added to [n]: the sum of their signs ([relative]) tells whether
          the value is a constant (0), an address in this section (1), or
          relative to the field (-1). *)
       let n = ref n in
       let symbols = ref [] in
       let relative = ref 0 in
       let positive_local = ref None in
       List.iter (fun (sign, label) ->
         if label = "." || local label then
           begin
             let position =
               if label = "." then offset else Hashtbl.find labels label in
             n := Int64.add !n (Int64.of_int (sign * position));
             relative := !relative + sign;
             if sign = 1 && label <> "." then
               positive_local := Some (label, position)
           end
         else
           symbols := (sign, label) :: !symbols) labels_terms;
       let address_kind target : Object.relocation_kind =
         match width with
         | 8 -> Absolute64
         | 4 -> Absolute32_signed
         | _ -> unsupported "%d-byte address of %s" width target in
       begin match !symbols, !relative, !positive_local with
       | [], 0, _ -> add_int ~width !n
       | [], 1, Some (target, position) ->
          (* The address of a label of this section. *)
          add_relocation ~offset ~kind:(address_kind target) ~target
            ~addend:(Int64.sub !n (Int64.of_int position));
          add_int ~width 0L
       | [], _, _ ->
          unsupported "position-dependent constant in section %s" name
       | [(1, target)], 0, _ ->
          add_relocation ~offset ~kind:(address_kind target) ~target
            ~addend:!n;
          add_int ~width 0L
       | [(1, target)], -1, _ when width = 4 ->
          (* [target - p + n], with the positions [p] resolved in [n]: the
             field is [target + n' - field]. *)
          add_relocation ~offset ~kind:Relative32 ~target
            ~addend:(Int64.add !n (Int64.of_int offset));
          add_int ~width 0L
       | _ -> unsupported "constant expression in section %s" name
       end
    | Raw s -> Buffer.add_string b s
    | Zeros n -> Buffer.add_string b (String.make n '\000')
    | Alignment n ->
       alignment := max !alignment n;
       Buffer.add_string b
         (padding ~executable (align_up offset n - offset))) items;
  let contents = Buffer.to_bytes b in
  List.iter (fun (offset, width, n) ->
    match width with
    | 1 -> Bytes.set_int8 contents offset (Int64.to_int n)
    | 2 -> Bytes.set_int16_le contents offset (Int64.to_int n)
    | 4 -> Bytes.set_int32_le contents offset (Int64.to_int32 n)
    | _ -> Bytes.set_int64_le contents offset n) !patches;
  let symbols =
    Hashtbl.fold (fun label position acc ->
      (label, { Object.section = index; position;
                global = Hashtbl.mem state.globals label }) :: acc) labels [] in
  { Object.name; contents = Bytes.to_string contents;
    alignment = !alignment;
    executable; relocations = List.rev !relocations }, symbols

let assemble (program : asm_program) : Object.t =
  let state = collect program in
  let sections_and_symbols =
    List.rev state.section_order |> List.mapi (fun index name ->
      encode_section state index (Hashtbl.find state.sections name)) in
  let symbols = List.concat_map snd sections_and_symbols in
  (* Symbols defined with [.set] as another symbol plus a constant. *)
  let aliases =
    Hashtbl.fold (fun name c acc ->
      match terms 1 c (0L, []) with
      | n, [(1, label)] ->
         begin match List.assoc_opt label symbols with
         | Some symbol ->
            (name, { symbol with position = symbol.position + Int64.to_int n;
                                 global = Hashtbl.mem state.globals name })
            :: acc
         | None -> acc
         end
      | _ -> acc) state.aliases [] in
  { Object.sections = Array.of_list (List.map fst sections_and_symbols);
    symbols = aliases @ symbols }
