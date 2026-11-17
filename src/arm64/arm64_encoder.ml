(* Internal assembler for arm64: encodes the assembly code produced by the
   compiler into machine code ([Object.t]), without running an external
   assembler.

   Unlike the amd64 backend, which builds an assembly program
   ([X86_ast.asm_program]), the arm64 backend ([Emit]) only prints the
   assembly code: the text is parsed here. Only the instructions and the
   directives emitted by the arm64 backend are supported, in the syntax of
   macOS (Mach-O) and of the other systems (ELF). The encoding follows the
   choices of the system assembler (for instance, [ldur] for the negative
   offsets of [ldr]), so that the code can be compared word for word with the
   output of [as] (see [tests/encoder]).

   The branches to labels of the same section are resolved here, as by the
   assembler of macOS (even for global symbols), and so are the differences
   between labels of the same section. The other references (to external
   symbols, to labels of other sections, the addresses of labels in data, and
   all the references by page of [adrp] and of the following instruction)
   produce relocations, resolved by [Loader] when the code is loaded. *)

exception Unsupported of string

let unsupported fmt = Printf.ksprintf (fun msg -> raise (Unsupported msg)) fmt

let () =
  Printexc.register_printer (function
    | Unsupported msg -> Some ("Arm64_encoder: unsupported " ^ msg)
    | _ -> None)

let macosx = Config.system = "macosx"

let text_section_name = if macosx then "__TEXT,__text" else ".text"

let data_section_name = if macosx then "__DATA,__data" else ".data"

let is_text_section (name : string) : bool =
  name = text_section_name || String.starts_with ~prefix:".text." name

(* Registers. The number 31 denotes either the stack pointer ([sp]) or the
   zero register ([xzr], [wzr]), depending on the instruction. *)

type register_kind = X | W | D | S

type register = {
    number : int;
    kind : register_kind;
    sp : bool;
  }

let parse_register (s : string) : register option =
  let numbered kind rest =
    match int_of_string_opt rest with
    | Some number when 0 <= number && number <= 30
                       || (number = 31 && (kind = D || kind = S)) ->
       Some { number; kind; sp = false }
    | _ -> None in
  match s with
  | "sp" -> Some { number = 31; kind = X; sp = true }
  | "xzr" -> Some { number = 31; kind = X; sp = false }
  | "wzr" -> Some { number = 31; kind = W; sp = false }
  | "" -> None
  | _ ->
     let rest = String.sub s 1 (String.length s - 1) in
     if rest = "" || not (String.for_all (fun c -> '0' <= c && c <= '9') rest)
     then None
     else
       match s.[0] with
       | 'x' -> numbered X rest
       | 'w' -> numbered W rest
       | 'd' -> numbered D rest
       | 's' -> numbered S rest
       | _ -> None

let register (s : string) : register =
  match parse_register s with
  | Some r -> r
  | None -> unsupported "register %S" s

let is_general (r : register) : bool = r.kind = X || r.kind = W

let general (s : string) : register =
  let r = register s in
  if not (is_general r) then unsupported "general register %S" s;
  r

let float_register (s : string) : register =
  let r = register s in
  if is_general r then unsupported "floating-point register %S" s;
  r

(* The [sf] bit of the instructions on general registers: 64-bit
   operation. *)
let sf (r : register) : int = if r.kind = X then 1 lsl 31 else 0

(* The [type] field of the floating-point instructions: double
   precision. *)
let ftype (r : register) : int = if r.kind = D then 1 lsl 22 else 0

let condition_code (s : string) : int =
  match s with
  | "eq" -> 0 | "ne" -> 1 | "cs" | "hs" -> 2 | "cc" | "lo" -> 3
  | "mi" -> 4 | "pl" -> 5 | "vs" -> 6 | "vc" -> 7 | "hi" -> 8 | "ls" -> 9
  | "ge" -> 10 | "lt" -> 11 | "gt" -> 12 | "le" -> 13 | "al" -> 14
  | _ -> unsupported "condition %S" s

let is_condition (s : string) : bool =
  match condition_code s with
  | _ -> true
  | exception Unsupported _ -> false

(* Expressions: a constant plus symbols (and the current position, [.])
   with coefficients. *)

type expression = {
    constant : int64;
    symbols : (int * string) list;
    dot : int;
  }

let constant (n : int64) : expression = { constant = n; symbols = []; dot = 0 }

let add (a : expression) (b : expression) : expression =
  { constant = Int64.add a.constant b.constant;
    symbols = a.symbols @ b.symbols; dot = a.dot + b.dot }

let neg (a : expression) : expression =
  { constant = Int64.neg a.constant;
    symbols = List.map (fun (c, s) -> (-c, s)) a.symbols; dot = - a.dot }

let is_symbol_char (c : char) : bool =
  match c with
  | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' | '$' | '.' -> true
  | _ -> false

let parse_expression (s : string) : expression =
  let n = String.length s in
  let pos = ref 0 in
  let rec skip () =
    if !pos < n && (s.[!pos] = ' ' || s.[!pos] = '\t') then (incr pos; skip ()) in
  let peek () = skip (); if !pos < n then Some s.[!pos] else None in
  let word () =
    let start = !pos in
    while !pos < n && is_symbol_char s.[!pos] do incr pos done;
    String.sub s start (!pos - start) in
  let rec expr () =
    let rec loop acc =
      match peek () with
      | Some '+' -> incr pos; loop (add acc (term ()))
      | Some '-' -> incr pos; loop (add acc (neg (term ())))
      | _ -> acc in
    loop (term ())
  and term () =
    match peek () with
    | Some '-' -> incr pos; neg (term ())
    | Some '+' -> incr pos; term ()
    | Some '(' ->
       incr pos;
       let e = expr () in
       if peek () <> Some ')' then unsupported "expression %S" s;
       incr pos;
       e
    | Some ('0'..'9') ->
       let w = word () in
       (* Decimal numbers up to 2^64 - 1, as [-9223372036854775808] is the
          negation of 2^63. *)
       begin match Int64.of_string_opt w, Int64.of_string_opt ("0u" ^ w) with
       | Some k, _ | None, Some k -> constant k
       | None, None -> unsupported "number %S" w
       end
    | Some c when is_symbol_char c ->
       let w = word () in
       if w = "." then { (constant 0L) with dot = 1 }
       else { (constant 0L) with symbols = [(1, w)] }
    | _ -> unsupported "expression %S" s in
  let e = expr () in
  if peek () <> None then unsupported "expression %S" s;
  e

(* A single symbol plus a constant, as referred to by relocations. *)
let symbol_reference (s : string) : string * int64 =
  match parse_expression s with
  | { symbols = [(1, name)]; dot = 0; constant } -> (name, constant)
  | _ -> unsupported "symbol reference %S" s

let parse_immediate (s : string) : int64 =
  let s = String.trim s in
  let s =
    if String.starts_with ~prefix:"#" s then String.sub s 1 (String.length s - 1)
    else s in
  match parse_expression s with
  | { symbols = []; dot = 0; constant } -> constant
  | _ -> unsupported "immediate %S" s

let immediate (s : string) : int = Int64.to_int (parse_immediate s)

(* References to symbols in operands. *)

type reference =
  | Page of string * int64
    (** [adrp]: the page of the symbol. *)
  | Page_offset of string * int64
    (** The offset of the symbol in its page. *)
  | Got_page of string
    (** [adrp]: the page of the entry of the symbol in the GOT. *)
  | Got_page_offset of string
    (** The offset of the entry of the symbol in the GOT in its page. *)

let parse_reference ~(adrp : bool) (s : string) : reference option =
  let s = String.trim s in
  let s =
    if String.starts_with ~prefix:"#" s then String.sub s 1 (String.length s - 1)
    else s in
  let chop_suffix suffix =
    if String.ends_with ~suffix s then
      Some (String.sub s 0 (String.length s - String.length suffix))
    else None in
  let chop_prefix prefix =
    if String.starts_with ~prefix s then
      let k = String.length prefix in
      Some (String.sub s k (String.length s - k))
    else None in
  match chop_suffix "@GOTPAGEOFF", chop_suffix "@GOTPAGE",
        chop_suffix "@PAGEOFF", chop_suffix "@PAGE" with
  | Some name, _, _, _ -> Some (Got_page_offset (String.trim name))
  | _, Some name, _, _ -> Some (Got_page (String.trim name))
  | _, _, Some e, _ -> let name, addend = symbol_reference e in
                       Some (Page_offset (name, addend))
  | _, _, _, Some e -> let name, addend = symbol_reference e in
                       Some (Page (name, addend))
  | None, None, None, None ->
     match chop_prefix ":got_lo12:", chop_prefix ":got:", chop_prefix ":lo12:" with
     | Some name, _, _ -> Some (Got_page_offset (String.trim name))
     | _, Some name, _ -> Some (Got_page (String.trim name))
     | _, _, Some e -> let name, addend = symbol_reference e in
                       Some (Page_offset (name, addend))
     | None, None, None ->
        (* On ELF, [adrp x, symbol] refers to the page of the symbol. *)
        if adrp && s <> "" && is_symbol_char s.[0]
           && not ('0' <= s.[0] && s.[0] <= '9') then
          let name, addend = symbol_reference s in
          Some (Page (name, addend))
        else None

(* Operands are separated by commas, except inside brackets. *)
let split_operands (s : string) : string list =
  let s = String.trim s in
  if s = "" then []
  else
    let parts = ref [] and depth = ref 0 and start = ref 0 in
    String.iteri (fun i c ->
      match c with
      | '[' -> incr depth
      | ']' -> decr depth
      | ',' when !depth = 0 ->
         parts := String.trim (String.sub s !start (i - !start)) :: !parts;
         start := i + 1
      | _ -> ()) s;
    List.rev
      (String.trim (String.sub s !start (String.length s - !start)) :: !parts)

(* Shifts of the register operands: [lsl #n], [lsr #n], [asr #n]. *)
let parse_shift (s : string) : (int * int) option =
  match String.index_opt s ' ' with
  | None -> None
  | Some i ->
     let kind = String.sub s 0 i
     and amount = String.sub s (i + 1) (String.length s - i - 1) in
     let code =
       match kind with
       | "lsl" -> Some 0 | "lsr" -> Some 1 | "asr" -> Some 2 | _ -> None in
     Option.map (fun code -> (code, immediate amount)) code

(* Memory operands: [[base]], [[base, offset]], [[base, offset]!]. *)

type offset =
  | Immediate of int
  | Reference of reference

type addressing =
  | Offset
  | Pre_index
  | Post_index of int

type memory = {
    base : register;
    offset : offset;
    addressing : addressing;
  }

let parse_memory (operands : string list) : memory =
  let inner, pre, post =
    match operands with
    | [m] | [m; _] ->
       let post =
         match operands with [_; p] -> Some (immediate p) | _ -> None in
       let m = String.trim m in
       let pre = String.ends_with ~suffix:"!" m in
       let m = if pre then String.sub m 0 (String.length m - 1) else m in
       if not (String.starts_with ~prefix:"[" m && String.ends_with ~suffix:"]" m)
       then unsupported "memory operand %S" m;
       String.sub m 1 (String.length m - 2), pre, post
    | _ -> unsupported "memory operand %s" (String.concat ", " operands) in
  let base, offset =
    match split_operands inner with
    | [base] -> base, Immediate 0
    | [base; offset] ->
       base,
       begin match parse_reference ~adrp:false offset with
       | Some r -> Reference r
       | None -> Immediate (immediate offset)
       end
    | _ -> unsupported "memory operand [%s]" inner in
  let base = general base in
  let addressing =
    match pre, post with
    | false, None -> Offset
    | true, None -> Pre_index
    | false, Some n when offset = Immediate 0 -> Post_index n
    | _ -> unsupported "addressing mode [%s]" inner in
  { base; offset; addressing }

(* Logical immediates: a pattern of 2, 4, ..., 64 bits, made of a rotated
   run of ones, replicated. Returns the fields N, immr and imms. *)
let logical_immediate (width : int) (v : int64) : (int * int * int) option =
  let v =
    if width = 32 then
      let low = Int64.logand v 0xFFFF_FFFFL in
      Int64.logor low (Int64.shift_left low 32)
    else v in
  if v = 0L || v = -1L then None
  else
    let mask size =
      if size = 64 then -1L else Int64.pred (Int64.shift_left 1L size) in
    let replicate pattern size =
      let rec loop acc n =
        if n >= 64 then acc
        else loop (Int64.logor acc (Int64.shift_left pattern n)) (n + size) in
      loop 0L 0 in
    let rotate_right x r size =
      if r = 0 then x
      else
        Int64.logand (mask size)
          (Int64.logor (Int64.shift_right_logical x r)
             (Int64.shift_left x (size - r))) in
    let popcount x =
      let rec loop x n = if x = 0L then n
                         else loop (Int64.logand x (Int64.pred x)) (n + 1) in
      loop x 0 in
    let rec find size =
      if size > width then None
      else
        let pattern = Int64.logand v (mask size) in
        if replicate pattern size <> v then find (size * 2)
        else
          let ones = popcount pattern in
          let base = Int64.pred (Int64.shift_left 1L ones) in
          let rec rotation r =
            if r >= size then None
            else if rotate_right base r size = pattern then Some r
            else rotation (r + 1) in
          match rotation 0 with
          | None -> None
          | Some immr ->
             let n = if size = 64 then 1 else 0 in
             let imms = ((lnot (size - 1)) lsl 1) land 0x3F lor (ones - 1) in
             Some (n, immr, imms) in
    find 2

(* The 8-bit immediate of [fmov] for a double, if it can be represented. *)
let float_immediate (x : float) : int option =
  let bits = Int64.bits_of_float x in
  let field shift width =
    Int64.to_int (Int64.shift_right_logical bits shift) land ((1 lsl width) - 1) in
  let imm8 = (field 63 1 lsl 7) lor (field 61 1 lsl 6) lor field 48 6 in
  let a = imm8 lsr 7 and b = (imm8 lsr 6) land 1 in
  let expanded =
    Int64.logor (Int64.shift_left (Int64.of_int a) 63)
      (Int64.logor (Int64.shift_left (Int64.of_int (1 - b)) 62)
         (Int64.logor (Int64.shift_left (if b = 1 then 0xFFL else 0L) 54)
            (Int64.shift_left (Int64.of_int (imm8 land 0x3F)) 48))) in
  if expanded = bits then Some imm8 else None

(* The state of the assembler. *)

type field =
  | Data of int
    (** A data field of the given size in bytes. *)
  | Branch26
    (** [b], [bl]. *)
  | Branch19
    (** [b.cond], [cbz], [cbnz]. *)
  | Branch14
    (** [tbz], [tbnz]. *)
  | Adr21
    (** [adr]. *)

type section = {
    name : string;
    index : int;
    contents : Buffer.t;
    mutable alignment : int;
    mutable relocations : Object.relocation list;
  }

type fixup = {
    section : section;
    offset : int;
    field : field;
    expression : expression;
  }

type state = {
    sections : (string, section) Hashtbl.t;
    mutable section_order : section list;
    mutable current : section;
    labels : (string, section * int) Hashtbl.t;
    globals : (string, unit) Hashtbl.t;
    mutable fixups : fixup list;
    mutable conditions : bool list;
      (** The conditions of the enclosing [.if] directives. *)
  }

let new_section (state : state) (name : string) : section =
  match Hashtbl.find_opt state.sections name with
  | Some section -> section
  | None ->
     let section = {
         name; index = Hashtbl.length state.sections;
         contents = Buffer.create 4096; alignment = 1; relocations = [] } in
     Hashtbl.add state.sections name section;
     state.section_order <- section :: state.section_order;
     section

let position (state : state) : int = Buffer.length state.current.contents

let emit_word (state : state) (word : int) : unit =
  Buffer.add_int32_le state.current.contents (Int32.of_int (word land 0xFFFF_FFFF))

let relocation (state : state) (kind : Object.relocation_kind)
      (target : string) (addend : int64) : unit =
  state.current.relocations <-
    { offset = position state; kind; target; addend }
    :: state.current.relocations

let fixup (state : state) (field : field) (expression : expression) : unit =
  state.fixups <-
    { section = state.current; offset = position state; field; expression }
    :: state.fixups

type value =
  | Constant of int64
  | Absolute of string * int64
    (** [target + addend] *)
  | Relative of string * int64
    (** [target + addend - field] *)

(* The value of an expression for a field at [offset] in [section]. Labels
   of the same section cancel each other out; a remaining label (or external
   symbol) gives an absolute value, or a relative one if the field (or a
   label of its section) is subtracted. *)
let evaluate (state : state) (section : section) (offset : int)
      (e : expression) : value =
  let constant = ref e.constant in
  let coefficients : (int, int * string option) Hashtbl.t = Hashtbl.create 4 in
  let add_known (s : section) position coefficient label =
    constant := Int64.add !constant (Int64.of_int (coefficient * position));
    let c, l =
      Option.value (Hashtbl.find_opt coefficients s.index) ~default:(0, None) in
    let l = if l = None && coefficient = 1 then label else l in
    Hashtbl.replace coefficients s.index (c + coefficient, l) in
  if e.dot <> 0 then add_known section offset e.dot None;
  let externals = ref [] in
  List.iter (fun (coefficient, name) ->
    match Hashtbl.find_opt state.labels name with
    | Some (s, position) -> add_known s position coefficient (Some name)
    | None -> externals := (coefficient, name) :: !externals) e.symbols;
  let known =
    Hashtbl.fold (fun index (c, l) acc ->
      if c = 0 then acc else (index, c, l) :: acc) coefficients [] in
  let position_of label = Int64.of_int (snd (Hashtbl.find state.labels label)) in
  let offset = Int64.of_int offset in
  match known, !externals with
  | [], [] -> Constant !constant
  | [(_, 1, Some label)], [] ->
     Absolute (label, Int64.sub !constant (position_of label))
  | [], [(1, name)] -> Absolute (name, !constant)
  | [(i, -1, _); (_, 1, Some label)], []
  | [(_, 1, Some label); (i, -1, _)], [] when i = section.index ->
     Relative (label, Int64.add (Int64.sub !constant (position_of label)) offset)
  | [(i, -1, _)], [(1, name)] when i = section.index ->
     Relative (name, Int64.add !constant offset)
  | _ -> unsupported "expression in section %s" section.name

let active (state : state) : bool = List.for_all Fun.id state.conditions

(* Data directives. *)

let parse_string (s : string) : string =
  let s = String.trim s in
  let n = String.length s in
  if n < 2 || s.[0] <> '"' || s.[n - 1] <> '"' then unsupported "string %s" s;
  let b = Buffer.create n in
  let i = ref 1 in
  while !i < n - 1 do
    let c = s.[!i] in
    if c = '\\' then
      begin
        incr i;
        let c = s.[!i] in
        if '0' <= c && c <= '7' then
          begin
            let code = ref 0 and digits = ref 0 in
            while !digits < 3 && !i < n - 1 && '0' <= s.[!i] && s.[!i] <= '7' do
              code := !code * 8 + Char.code s.[!i] - Char.code '0';
              incr digits; incr i
            done;
            Buffer.add_char b (Char.chr (!code land 0xFF))
          end
        else
          begin
            Buffer.add_char b
              (match c with
               | 'n' -> '\n' | 't' -> '\t' | 'r' -> '\r' | 'b' -> '\b'
               | c -> c);
            incr i
          end
      end
    else
      begin
        Buffer.add_char b c;
        incr i
      end
  done;
  Buffer.contents b

let nop = 0xD503201F

let align (state : state) (alignment : int) : unit =
  let section = state.current in
  section.alignment <- max section.alignment alignment;
  let padding =
    (alignment - Buffer.length section.contents mod alignment) mod alignment in
  if is_text_section section.name && padding mod 4 = 0
     && Buffer.length section.contents mod 4 = 0 then
    for _ = 1 to padding / 4 do emit_word state nop done
  else
    Buffer.add_string section.contents (String.make padding '\000')

let data (state : state) (size : int) (operands : string) : unit =
  List.iter (fun operand ->
    let e = parse_expression operand in
    if e.symbols = [] && e.dot = 0 then
      begin
        let b = state.current.contents in
        match size with
        | 1 -> Buffer.add_uint8 b (Int64.to_int e.constant land 0xFF)
        | 2 -> Buffer.add_uint16_le b (Int64.to_int e.constant land 0xFFFF)
        | 4 -> Buffer.add_int32_le b (Int64.to_int32 e.constant)
        | _ -> Buffer.add_int64_le b e.constant
      end
    else
      begin
        fixup state (Data size) e;
        Buffer.add_string state.current.contents (String.make size '\000')
      end) (split_operands operands)

let directive (state : state) (name : string) (operands : string) : unit =
  match name with
  (* The comparisons of an absolute expression with zero, as by the GNU
     assembler. Since OCaml 5.5, the backend checks the sizes of the
     instructions with [.ifgt (. - label) - size] and [.error]. *)
  | ".ifne" | ".ifeq" | ".if" | ".ifgt" | ".ifge" | ".iflt" | ".ifle" ->
     let value =
       if active state then
         match evaluate state state.current (position state)
                 (parse_expression operands) with
         | Constant n -> n
         | _ -> unsupported "condition %s" operands
       else 0L in
     let condition =
       match name with
       | ".ifeq" -> value = 0L
       | ".ifgt" -> value > 0L
       | ".ifge" -> value >= 0L
       | ".iflt" -> value < 0L
       | ".ifle" -> value <= 0L
       | _ -> value <> 0L in
     state.conditions <- condition :: state.conditions
  | ".endif" ->
     begin match state.conditions with
     | _ :: rest -> state.conditions <- rest
     | [] -> unsupported ".endif without .if"
     end
  | _ when not (active state) -> ()
  | ".error" -> unsupported "assembly: .error %s" operands
  | ".text" -> state.current <- new_section state text_section_name
  | ".data" -> state.current <- new_section state data_section_name
  | ".section" ->
     let name =
       if String.starts_with ~prefix:"__" operands then
         (* Mach-O: segment,section[,type] *)
         match split_operands operands with
         | segment :: section :: _ -> segment ^ "," ^ section
         | _ -> unsupported ".section %s" operands
       else
         match split_operands operands with
         | name :: _ -> name
         | [] -> unsupported ".section %s" operands in
     state.current <- new_section state name
  | ".globl" | ".global" -> Hashtbl.replace state.globals (String.trim operands) ()
  | ".align" | ".p2align" -> align state (1 lsl immediate operands)
  | ".balign" -> align state (immediate operands)
  | ".byte" -> data state 1 operands
  | ".short" | ".hword" -> data state 2 operands
  | ".long" | ".word" | ".int" -> data state 4 operands
  | ".quad" | ".xword" -> data state 8 operands
  | ".ascii" -> Buffer.add_string state.current.contents (parse_string operands)
  | ".asciz" | ".string" ->
     Buffer.add_string state.current.contents (parse_string operands);
     Buffer.add_char state.current.contents '\000'
  | ".space" | ".skip" | ".zero" ->
     Buffer.add_string state.current.contents (String.make (immediate operands) '\000')
  | ".file" | ".loc" | ".type" | ".size" | ".ident" | ".build_version"
  | ".subsections_via_symbols" -> ()
  (* The architecture that the assembler accepts (since OCaml 5.6,
     [.arch armv8.4-a]): the instructions are encoded as they are. *)
  | ".arch" -> ()
  | _ when String.starts_with ~prefix:".cfi_" name ->
     (* The unwinding information is not needed: the runtime walks the stack
        with the frame tables. *)
     ()
  | _ -> unsupported "directive %s" name

(* Instructions. *)

let page_offset_relocation (state : state) (shift : int) (r : reference) : unit =
  match r with
  | Page_offset (name, addend) ->
     relocation state (Arm64_pageoff12 shift) name addend
  | Got_page_offset name ->
     if shift <> 3 then unsupported "GOT reference";
     relocation state Arm64_got_pageoff12 name 0L
  | Page _ | Got_page _ -> unsupported "page reference in an offset"

(* [add], [sub], [adds], [subs] (and [cmp], [cmn], [mov] to or from [sp]). *)
let add_sub (state : state) ~(op : int) ~(s : int) (rd : register)
      (rn : register) (operands : string list) : unit =
  let base = sf rd lor (op lsl 30) lor (s lsl 29) in
  match operands with
  | [operand] when not (Option.is_some (parse_register operand)) ->
     begin match parse_reference ~adrp:false operand with
     | Some r ->
        page_offset_relocation state 0 r;
        emit_word state (base lor 0x11000000 lor (rn.number lsl 5) lor rd.number)
     | None ->
        let n = immediate operand in
        (* A negative immediate is encoded by the opposite instruction
           ([add x, y, #-4] is [sub x, y, #4]), as by the assembler. *)
        let base, n =
          if n < 0 then base lxor (1 lsl 30), - n else base, n in
        let sh, imm =
          if 0 <= n && n <= 0xFFF then 0, n
          else if n land 0xFFF = 0 && 0 <= n && n lsr 12 <= 0xFFF then 1, n lsr 12
          else unsupported "immediate %d" n in
        emit_word state
          (base lor 0x11000000 lor (sh lsl 22) lor (imm lsl 10)
           lor (rn.number lsl 5) lor rd.number)
     end
  | rm :: shift ->
     let rm = general rm in
     let kind, amount =
       match shift with
       | [] -> 0, 0
       | [shift] ->
          begin match parse_shift shift with
          | Some s -> s
          | None -> unsupported "shift %S" shift
          end
       | _ -> unsupported "operands of add/sub" in
     if rd.sp || rn.sp then
       begin
         (* Extended register, [uxtx] (or [uxtw]). *)
         if kind <> 0 || amount > 4 then unsupported "shift with sp";
         let option = if rm.kind = X then 3 else 2 in
         emit_word state
           (base lor 0x0B200000 lor (rm.number lsl 16) lor (option lsl 13)
            lor (amount lsl 10) lor (rn.number lsl 5) lor rd.number)
       end
     else
       emit_word state
         (base lor 0x0B000000 lor (kind lsl 22) lor (rm.number lsl 16)
          lor (amount lsl 10) lor (rn.number lsl 5) lor rd.number)
  | [] -> unsupported "operands of add/sub"

(* [and], [orr], [eor], [ands] with a shifted register. *)
let logical_register (state : state) ~(opc : int) (rd : register)
      (rn : register) (rm : register) (kind : int) (amount : int) : unit =
  emit_word state
    (sf rd lor (opc lsl 29) lor 0x0A000000 lor (kind lsl 22)
     lor (rm.number lsl 16) lor (amount lsl 10) lor (rn.number lsl 5)
     lor rd.number)

(* [and], [orr], [eor], [ands]. *)
let logical (state : state) ~(opc : int) (rd : register) (rn : register)
      (operands : string list) : unit =
  let base = sf rd lor (opc lsl 29) in
  match operands with
  | [operand] when not (Option.is_some (parse_register operand)) ->
     let v = parse_immediate operand in
     begin match logical_immediate (if rd.kind = X then 64 else 32) v with
     | Some (n, immr, imms) ->
        emit_word state
          (base lor 0x12000000 lor (n lsl 22) lor (immr lsl 16) lor (imms lsl 10)
           lor (rn.number lsl 5) lor rd.number)
     | None -> unsupported "logical immediate %Ld" v
     end
  | rm :: shift ->
     let rm = general rm in
     let kind, amount =
       match shift with
       | [] -> 0, 0
       | [shift] ->
          begin match parse_shift shift with
          | Some s -> s
          | None -> unsupported "shift %S" shift
          end
       | _ -> unsupported "operands of a logical instruction" in
     logical_register state ~opc rd rn rm kind amount
  | [] -> unsupported "operands of a logical instruction"

(* [movz] (opc 2), [movn] (opc 0), [movk] (opc 3). *)
let move_wide (state : state) ~(opc : int) (rd : register) (imm : int)
      (shift : int) : unit =
  if imm < 0 || imm > 0xFFFF || shift mod 16 <> 0
     || shift >= (if rd.kind = X then 64 else 32) then
    unsupported "wide immediate %d, lsl #%d" imm shift;
  emit_word state
    (sf rd lor (opc lsl 29) lor 0x12800000 lor ((shift / 16) lsl 21)
     lor (imm lsl 5) lor rd.number)

(* [mov rd, #imm]: [movz], [movn] or [orr], as chosen by the assembler. *)
let move_immediate (state : state) (rd : register) (v : int64) : unit =
  let width = if rd.kind = X then 64 else 32 in
  let v = if width = 32 then Int64.logand v 0xFFFF_FFFFL else v in
  let mask = if width = 32 then 0xFFFF_FFFFL else -1L in
  let single_halfword x =
    List.find_opt (fun shift ->
      Int64.logand x (Int64.lognot (Int64.shift_left 0xFFFFL shift)) = 0L)
      (List.init (width / 16) (fun i -> 16 * i)) in
  match single_halfword v with
  | Some shift ->
     move_wide state ~opc:2 rd
       (Int64.to_int (Int64.shift_right_logical v shift) land 0xFFFF) shift
  | None ->
     let inverted = Int64.logand (Int64.lognot v) mask in
     match single_halfword inverted with
     | Some shift ->
        move_wide state ~opc:0 rd
          (Int64.to_int (Int64.shift_right_logical inverted shift) land 0xFFFF)
          shift
     | None ->
        logical state ~opc:1 rd { rd with number = 31; sp = false }
          ["#" ^ Int64.to_string v]

(* [sbfm] (opc 0), [ubfm] (opc 2). *)
let bitfield (state : state) ~(opc : int) (rd : register) (rn : register)
      (immr : int) (imms : int) : unit =
  let n = if rd.kind = X then 1 else 0 in
  emit_word state
    (sf rd lor (opc lsl 29) lor 0x13000000 lor (n lsl 22) lor (immr lsl 16)
     lor (imms lsl 10) lor (rn.number lsl 5) lor rd.number)

(* Loads and stores of a register: the size of the access (log2) and the
   encoding of the unsigned offset form. *)
let load_store_encoding (mnemonic : string) (rt : register) : int * int =
  match mnemonic, rt.kind with
  | "ldr", X -> 3, 0xF9400000 | "str", X -> 3, 0xF9000000
  | "ldr", W -> 2, 0xB9400000 | "str", W -> 2, 0xB9000000
  | "ldr", D -> 3, 0xFD400000 | "str", D -> 3, 0xFD000000
  | "ldr", S -> 2, 0xBD400000 | "str", S -> 2, 0xBD000000
  | "ldrb", W -> 0, 0x39400000 | "strb", W -> 0, 0x39000000
  | "ldrh", W -> 1, 0x79400000 | "strh", W -> 1, 0x79000000
  | "ldrsb", X -> 0, 0x39800000 | "ldrsh", X -> 1, 0x79800000
  | "ldrsw", X -> 2, 0xB9800000
  | "ldrsb", W -> 0, 0x39C00000 | "ldrsh", W -> 1, 0x79C00000
  | _ -> unsupported "%s with register of this size" mnemonic

let load_store (state : state) (mnemonic : string) (rt : register)
      (m : memory) : unit =
  let size, encoding = load_store_encoding mnemonic rt in
  let unscaled offset =
    if offset < -256 || offset > 255 then unsupported "offset %d" offset;
    (encoding land lnot 0x01000000) lor ((offset land 0x1FF) lsl 12) in
  let word =
    match m.addressing, m.offset with
    | Offset, Immediate offset ->
       if offset >= 0 && offset land ((1 lsl size) - 1) = 0
          && offset lsr size <= 0xFFF then
         encoding lor ((offset lsr size) lsl 10)
       else unscaled offset
    | Offset, Reference r ->
       page_offset_relocation state size r;
       encoding
    | Pre_index, Immediate offset -> unscaled offset lor 0xC00
    | Post_index offset, Immediate 0 -> unscaled offset lor 0x400
    | _ -> unsupported "addressing mode of %s" mnemonic in
  emit_word state (word lor (m.base.number lsl 5) lor rt.number)

(* [ldp], [stp] of 64-bit registers. *)
let load_store_pair (state : state) ~(load : bool) (rt : register)
      (rt2 : register) (m : memory) : unit =
  if rt.kind <> X || rt2.kind <> X then unsupported "ldp/stp of these registers";
  let l = if load then 1 lsl 22 else 0 in
  let encoding, offset =
    match m.addressing, m.offset with
    | Offset, Immediate offset -> 0xA9000000, offset
    | Pre_index, Immediate offset -> 0xA9800000, offset
    | Post_index offset, Immediate 0 -> 0xA8800000, offset
    | _ -> unsupported "addressing mode of ldp/stp" in
  if offset land 7 <> 0 || offset < -512 || offset > 504 then
    unsupported "ldp/stp offset %d" offset;
  emit_word state
    (encoding lor l lor (((offset asr 3) land 0x7F) lsl 15)
     lor (rt2.number lsl 10) lor (m.base.number lsl 5) lor rt.number)

(* Instructions with a label: the field is filled in later. *)
let branch (state : state) (field : field) (word : int) (target : string) : unit =
  fixup state field (parse_expression target);
  emit_word state word

let float_binary (state : state) (base : int) (operands : string list) : unit =
  match operands with
  | [rd; rn; rm] ->
     let rd = float_register rd and rn = float_register rn
     and rm = float_register rm in
     emit_word state
       (base lor ftype rd lor (rm.number lsl 16) lor (rn.number lsl 5)
        lor rd.number)
  | _ -> unsupported "operands of a floating-point instruction"

let float_unary (state : state) (base : int) (operands : string list) : unit =
  match operands with
  | [rd; rn] ->
     let rd = float_register rd and rn = float_register rn in
     emit_word state (base lor ftype rd lor (rn.number lsl 5) lor rd.number)
  | _ -> unsupported "operands of a floating-point instruction"

let float_ternary (state : state) (base : int) (operands : string list) : unit =
  match operands with
  | [rd; rn; rm; ra] ->
     let rd = float_register rd and rn = float_register rn
     and rm = float_register rm and ra = float_register ra in
     emit_word state
       (base lor ftype rd lor (rm.number lsl 16) lor (ra.number lsl 10)
        lor (rn.number lsl 5) lor rd.number)
  | _ -> unsupported "operands of a floating-point instruction"

let instruction (state : state) (mnemonic : string) (operands : string list) : unit =
  let zr (r : register) = { r with number = 31; sp = false } in
  match mnemonic, operands with
  | ("add" | "sub" | "adds" | "subs"), rd :: rn :: rest ->
     let op = if mnemonic.[0] = 's' then 1 else 0 in
     let s = if String.length mnemonic = 4 then 1 else 0 in
     add_sub state ~op ~s (general rd) (general rn) rest
  | ("cmp" | "cmn"), rn :: rest ->
     let rn = general rn in
     add_sub state ~op:(if mnemonic = "cmp" then 1 else 0) ~s:1 (zr rn) rn rest
  | "mov", [rd; rs] ->
     begin match parse_register rd, parse_register rs with
     | Some rd, Some rs when is_general rd && is_general rs ->
        if rd.sp || rs.sp then add_sub state ~op:0 ~s:0 rd rs ["#0"]
        else logical_register state ~opc:1 rd (zr rd) rs 0 0
     | Some rd, None when is_general rd -> move_immediate state rd (parse_immediate rs)
     | _ -> unsupported "mov %s, %s" rd rs
     end
  | ("movz" | "movn" | "movk"), rd :: imm :: shift ->
     let opc = match mnemonic with "movn" -> 0 | "movz" -> 2 | _ -> 3 in
     let shift =
       match shift with
       | [] -> 0
       | [s] ->
          begin match parse_shift s with
          | Some (0, amount) -> amount
          | _ -> unsupported "shift %S" s
          end
       | _ -> unsupported "operands of %s" mnemonic in
     move_wide state ~opc (general rd) (immediate imm) shift
  | ("and" | "orr" | "eor" | "ands"), rd :: rn :: rest ->
     let opc = match mnemonic with
       | "and" -> 0 | "orr" -> 1 | "eor" -> 2 | _ -> 3 in
     logical state ~opc (general rd) (general rn) rest
  | "tst", rn :: rest ->
     let rn = general rn in
     logical state ~opc:3 (zr rn) rn rest
  | ("lsl" | "lsr" | "asr"), [rd; rn; rm] ->
     let rd = general rd and rn = general rn in
     begin match parse_register rm with
     | Some rm ->
        let op2 = match mnemonic with "lsl" -> 0 | "lsr" -> 1 | _ -> 2 in
        emit_word state
          (sf rd lor 0x1AC02000 lor (rm.number lsl 16) lor (op2 lsl 10)
           lor (rn.number lsl 5) lor rd.number)
     | None ->
        let width = if rd.kind = X then 64 else 32 in
        let s = immediate rm in
        if s < 0 || s >= width then unsupported "shift %d" s;
        match mnemonic with
        | "lsl" -> bitfield state ~opc:2 rd rn ((width - s) mod width) (width - 1 - s)
        | "lsr" -> bitfield state ~opc:2 rd rn s (width - 1)
        | _ -> bitfield state ~opc:0 rd rn s (width - 1)
     end
  | ("sbfm" | "ubfm"), [rd; rn; immr; imms] ->
     bitfield state ~opc:(if mnemonic = "sbfm" then 0 else 2) (general rd)
       (general rn) (immediate immr) (immediate imms)
  | "mul", [rd; rn; rm] ->
     let rd = general rd and rn = general rn and rm = general rm in
     emit_word state
       (sf rd lor 0x1B000000 lor (rm.number lsl 16) lor (31 lsl 10)
        lor (rn.number lsl 5) lor rd.number)
  | ("madd" | "msub"), [rd; rn; rm; ra] ->
     let rd = general rd and rn = general rn and rm = general rm
     and ra = general ra in
     let o0 = if mnemonic = "msub" then 1 lsl 15 else 0 in
     emit_word state
       (sf rd lor 0x1B000000 lor o0 lor (rm.number lsl 16) lor (ra.number lsl 10)
        lor (rn.number lsl 5) lor rd.number)
  | "smulh", [rd; rn; rm] ->
     let rd = general rd and rn = general rn and rm = general rm in
     emit_word state
       (0x9B400000 lor (rm.number lsl 16) lor (31 lsl 10) lor (rn.number lsl 5)
        lor rd.number)
  | ("sdiv" | "udiv"), [rd; rn; rm] ->
     let rd = general rd and rn = general rn and rm = general rm in
     let base = if mnemonic = "sdiv" then 0x1AC00C00 else 0x1AC00800 in
     emit_word state
       (sf rd lor base lor (rm.number lsl 16) lor (rn.number lsl 5) lor rd.number)
  (* [clz] and [rbit]: since OCaml 5.6 ([Iclz], [Ictz]). *)
  | ("rev" | "rev16" | "clz" | "rbit"), [rd; rn] ->
     let rd = general rd and rn = general rn in
     let base =
       match mnemonic, rd.kind with
       | "rev", X -> 0xDAC00C00 | "rev", _ -> 0x5AC00800
       | "clz", X -> 0xDAC01000 | "clz", _ -> 0x5AC01000
       | "rbit", X -> 0xDAC00000 | "rbit", _ -> 0x5AC00000
       | _, X -> 0xDAC00400 | _ -> 0x5AC00400 in
     emit_word state (base lor (rn.number lsl 5) lor rd.number)
  | "cset", [rd; cond] ->
     let rd = general rd in
     let cond = condition_code cond lxor 1 in
     emit_word state
       (sf rd lor 0x1A800400 lor (31 lsl 16) lor (cond lsl 12) lor (31 lsl 5)
        lor rd.number)
  | ("ldr" | "str" | "ldrb" | "strb" | "ldrh" | "strh" | "ldrsb" | "ldrsh"
    | "ldrsw"), rt :: memory ->
     load_store state mnemonic (register rt) (parse_memory memory)
  | ("ldp" | "stp"), rt :: rt2 :: memory ->
     load_store_pair state ~load:(mnemonic = "ldp") (register rt) (register rt2)
       (parse_memory memory)
  | ("ldar" | "stlr" | "ldaxr"), [rt; memory] ->
     let rt = general rt in
     let m = parse_memory [memory] in
     if m.offset <> Immediate 0 || m.addressing <> Offset then
       unsupported "addressing mode of %s" mnemonic;
     let base =
       match mnemonic, rt.kind with
       | "ldar", X -> 0xC8DFFC00 | "ldar", _ -> 0x88DFFC00
       | "ldaxr", X -> 0xC85FFC00 | "ldaxr", _ -> 0x885FFC00
       | _, X -> 0xC89FFC00 | _ -> 0x889FFC00 in
     emit_word state (base lor (m.base.number lsl 5) lor rt.number)
  (* Since OCaml 5.6: the loop of [Atomic.fetch_and_add] without LSE. *)
  | "stlxr", [rs; rt; memory] ->
     let rs = general rs and rt = general rt in
     let m = parse_memory [memory] in
     if m.offset <> Immediate 0 || m.addressing <> Offset then
       unsupported "addressing mode of %s" mnemonic;
     let base = if rt.kind = X then 0xC800FC00 else 0x8800FC00 in
     emit_word state
       (base lor (rs.number lsl 16) lor (m.base.number lsl 5) lor rt.number)
  (* Since OCaml 5.6 (FEAT_LRCPC2, [-flrcpc2]): the store-release of a field,
     with an unscaled offset. *)
  | "stlur", [rt; memory] ->
     let rt = general rt in
     let m = parse_memory [memory] in
     let offset =
       match m.offset, m.addressing with
       | Immediate n, Offset when n >= -256 && n <= 255 -> n
       | _ -> unsupported "addressing mode of %s" mnemonic in
     let base = if rt.kind = X then 0xD9000000 else 0x99000000 in
     emit_word state
       (base lor ((offset land 0x1FF) lsl 12) lor (m.base.number lsl 5)
        lor rt.number)
  | "b", [target] -> branch state Branch26 0x14000000 target
  | "bl", [target] -> branch state Branch26 0x94000000 target
  | ("cbz" | "cbnz"), [rt; target] ->
     let rt = general rt in
     let base = if mnemonic = "cbz" then 0x34000000 else 0x35000000 in
     branch state Branch19 (sf rt lor base lor rt.number) target
  | ("tbz" | "tbnz"), [rt; bit; target] ->
     let rt = general rt and bit = immediate bit in
     let base = if mnemonic = "tbz" then 0x36000000 else 0x37000000 in
     branch state Branch14
       (((bit lsr 5) lsl 31) lor base lor ((bit land 31) lsl 19) lor rt.number)
       target
  | ("br" | "blr"), [rn] ->
     let rn = general rn in
     let base = if mnemonic = "br" then 0xD61F0000 else 0xD63F0000 in
     emit_word state (base lor (rn.number lsl 5))
  | "ret", [] -> emit_word state 0xD65F03C0
  | "ret", [rn] -> emit_word state (0xD65F0000 lor ((general rn).number lsl 5))
  | "adr", [rd; target] -> branch state Adr21 (0x10000000 lor (general rd).number) target
  | "adrp", [rd; target] ->
     let rd = general rd in
     begin match parse_reference ~adrp:true target with
     | Some (Page (name, addend)) -> relocation state Arm64_page21 name addend
     | Some (Got_page name) -> relocation state Arm64_got_page21 name 0L
     | _ -> unsupported "adrp %s" target
     end;
     emit_word state (0x90000000 lor rd.number)
  | "nop", [] -> emit_word state nop
  | "dmb", [option] ->
     let crm =
       match option with
       | "ish" -> 0xB | "ishld" -> 0x9 | "ishst" -> 0xA | "sy" -> 0xF
       | "ld" -> 0xD | "st" -> 0xE
       | _ -> unsupported "dmb %s" option in
     emit_word state (0xD50330BF lor (crm lsl 8))
  | "fmov", [rd; rs] ->
     let rd = register rd in
     begin match parse_register rs with
     | Some rs when not (is_general rd) && not (is_general rs) ->
        emit_word state (0x1E204000 lor ftype rd lor (rs.number lsl 5) lor rd.number)
     | Some rs when not (is_general rd) && rs.kind = X && rd.kind = D ->
        emit_word state (0x9E670000 lor (rs.number lsl 5) lor rd.number)
     | Some rs when rd.kind = X && rs.kind = D ->
        emit_word state (0x9E660000 lor (rs.number lsl 5) lor rd.number)
     | None when not (is_general rd) ->
        let s = String.trim rs in
        let s = if String.starts_with ~prefix:"#" s
                then String.sub s 1 (String.length s - 1) else s in
        begin match Option.bind (float_of_string_opt s) float_immediate with
        | Some imm8 ->
           emit_word state (0x1E201000 lor ftype rd lor (imm8 lsl 13) lor rd.number)
        | None -> unsupported "fmov immediate %s" rs
        end
     | _ -> unsupported "fmov %s" rs
     end
  | "fadd", _ -> float_binary state 0x1E202800 operands
  | "fsub", _ -> float_binary state 0x1E203800 operands
  | "fmul", _ -> float_binary state 0x1E200800 operands
  | "fdiv", _ -> float_binary state 0x1E201800 operands
  | "fnmul", _ -> float_binary state 0x1E208800 operands
  | "fabs", _ -> float_unary state 0x1E20C000 operands
  | "fneg", _ -> float_unary state 0x1E214000 operands
  | "fsqrt", _ -> float_unary state 0x1E21C000 operands
  (* Since OCaml 5.6 ([Iroundf]): [Float.round], [trunc], [ceil], [floor]. *)
  | "frinta", _ -> float_unary state 0x1E264000 operands
  | "frintz", _ -> float_unary state 0x1E25C000 operands
  | "frintp", _ -> float_unary state 0x1E24C000 operands
  | "frintm", _ -> float_unary state 0x1E254000 operands
  | "fmadd", _ -> float_ternary state 0x1F000000 operands
  | "fmsub", _ -> float_ternary state 0x1F008000 operands
  | "fnmadd", _ -> float_ternary state 0x1F200000 operands
  | "fnmsub", _ -> float_ternary state 0x1F208000 operands
  | "fcmp", [rn; rm] ->
     let rn = float_register rn and rm = float_register rm in
     emit_word state (0x1E202000 lor ftype rn lor (rm.number lsl 16) lor (rn.number lsl 5))
  | "fcvt", [rd; rn] ->
     let rd = float_register rd and rn = float_register rn in
     begin match rd.kind, rn.kind with
     | D, S -> emit_word state (0x1E22C000 lor (rn.number lsl 5) lor rd.number)
     | S, D -> emit_word state (0x1E624000 lor (rn.number lsl 5) lor rd.number)
     | _ -> unsupported "fcvt"
     end
  | "scvtf", [rd; rn] ->
     let rd = float_register rd and rn = general rn in
     emit_word state (sf rn lor 0x1E220000 lor ftype rd lor (rn.number lsl 5) lor rd.number)
  | "fcvtzs", [rd; rn] ->
     let rd = general rd and rn = float_register rn in
     emit_word state (sf rd lor 0x1E380000 lor ftype rn lor (rn.number lsl 5) lor rd.number)
  | _ ->
     (* Conditional branches: [b.cond], or [bcond]. *)
     let n = String.length mnemonic in
     let condition =
       if n = 4 && String.starts_with ~prefix:"b." mnemonic then
         Some (String.sub mnemonic 2 2)
       else if n = 3 && mnemonic.[0] = 'b' && is_condition (String.sub mnemonic 1 2)
       then Some (String.sub mnemonic 1 2)
       else None in
     match condition, operands with
     | Some cond, [target] ->
        branch state Branch19 (0x54000000 lor condition_code cond) target
     | _ -> unsupported "instruction %s %s" mnemonic (String.concat ", " operands)

(* Lines: labels, then a directive or an instruction. *)

let rec line (state : state) (s : string) : unit =
  let s = String.trim s in
  if s <> "" then
    let n = String.length s in
    let i = ref 0 in
    while !i < n && is_symbol_char s.[!i] do incr i done;
    if !i > 0 && !i < n && s.[!i] = ':' then
      begin
        let label = String.sub s 0 !i in
        if active state then
          begin
            if Hashtbl.mem state.labels label then
              unsupported "label %s defined twice" label;
            Hashtbl.add state.labels label (state.current, position state)
          end;
        line state (String.sub s (!i + 1) (n - !i - 1))
      end
    else
      let mnemonic, operands =
        match String.index_from_opt s 0 ' ', String.index_from_opt s 0 '\t' with
        | None, None -> s, ""
        | Some a, None | None, Some a -> String.sub s 0 a, String.sub s a (n - a)
        | Some a, Some b ->
           let a = min a b in String.sub s 0 a, String.sub s a (n - a) in
      let operands = String.trim operands in
      if mnemonic.[0] = '.' then directive state mnemonic operands
      else if active state then
        instruction state (String.lowercase_ascii mnemonic)
          (split_operands operands)

(* Fills in the fields that refer to labels, or produces relocations. *)
let resolve (state : state) (contents : Bytes.t array) (f : fixup) : unit =
  let b = contents.(f.section.index) in
  let patch mask value =
    let word = Int32.to_int (Bytes.get_int32_le b f.offset) land 0xFFFF_FFFF in
    let word = (word land lnot mask) lor (value land mask) in
    Bytes.set_int32_le b f.offset (Int32.of_int word) in
  let relocate kind target addend =
    f.section.relocations <-
      { offset = f.offset; kind; target; addend } :: f.section.relocations in
  match f.field with
  | Data size ->
     begin match evaluate state f.section f.offset f.expression, size with
     | Constant n, 1 -> Bytes.set_uint8 b f.offset (Int64.to_int n land 0xFF)
     | Constant n, 2 -> Bytes.set_uint16_le b f.offset (Int64.to_int n land 0xFFFF)
     | Constant n, 4 -> Bytes.set_int32_le b f.offset (Int64.to_int32 n)
     | Constant n, _ -> Bytes.set_int64_le b f.offset n
     | Absolute (target, addend), 8 -> relocate Absolute64 target addend
     | Relative (target, addend), 4 -> relocate Relative32 target addend
     | _ -> unsupported "data reference in section %s" f.section.name
     end
  | Branch26 | Branch19 | Branch14 | Adr21 ->
     let target, addend =
       match f.expression with
       | { symbols = [(1, name)]; dot = 0; constant } -> name, constant
       | _ -> unsupported "branch target" in
     let local =
       match Hashtbl.find_opt state.labels target with
       | Some (section, position) when section == f.section ->
          Some (position + Int64.to_int addend - f.offset)
       | _ -> None in
     let bits, shift, mask =
       match f.field with
       | Branch26 -> 26, 0, 0x03FFFFFF
       | Branch19 -> 19, 5, 0x00FFFFE0
       | Branch14 -> 14, 5, 0x0007FFE0
       | _ -> 21, 5, 0x60FFFFE0 in
     match local, f.field with
     | Some delta, Adr21 ->
        if delta < - (1 lsl 20) || delta >= 1 lsl 20 then
          unsupported "adr to %s out of range" target;
        patch mask (((delta land 3) lsl 29) lor (((delta asr 2) land 0x7FFFF) lsl 5))
     | Some delta, _ ->
        let words = delta asr 2 in
        if delta land 3 <> 0 || words < - (1 lsl (bits - 1))
           || words >= 1 lsl (bits - 1) then
          unsupported "branch to %s out of range" target;
        patch mask ((words land ((1 lsl bits) - 1)) lsl shift)
     | None, Branch26 -> relocate Arm64_branch26 target addend
     | None, _ -> unsupported "branch to %s (not a local label)" target

let assemble (text : string) : Object.t =
  let placeholder = {
      name = ""; index = -1; contents = Buffer.create 0; alignment = 1;
      relocations = [] } in
  let state = {
      sections = Hashtbl.create 8; section_order = []; current = placeholder;
      labels = Hashtbl.create 1024; globals = Hashtbl.create 256; fixups = [];
      conditions = [] } in
  state.current <- new_section state text_section_name;
  List.iter (line state) (String.split_on_char '\n' text);
  if state.conditions <> [] then unsupported ".if without .endif";
  let sections = List.rev state.section_order in
  let contents =
    Array.of_list (List.map (fun s -> Buffer.to_bytes s.contents) sections) in
  List.iter (resolve state contents) (List.rev state.fixups);
  let symbols =
    Hashtbl.fold (fun name (section, position) acc ->
      (name, { Object.section = section.index; position;
               global = Hashtbl.mem state.globals name }) :: acc)
      state.labels [] in
  { sections =
      Array.of_list (List.map (fun (s : section) ->
        { Object.name = s.name;
          contents = Bytes.unsafe_to_string contents.(s.index);
          alignment = s.alignment; executable = is_text_section s.name;
          relocations = List.rev s.relocations }) sections);
    symbols }
