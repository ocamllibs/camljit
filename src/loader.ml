(* Loader of the machine code produced by [X86_encoder] (on amd64) and
   [Arm64_encoder] (on arm64): the units are placed in memory, relocated,
   registered in the runtime and run, without linking a shared library nor
   loading it with the dynamic linker.

   The code and the data of each unit are mapped together: executable
   sections, followed by the stubs of the calls to external functions; then,
   on the next page, the other sections, followed by the global offset table
   (GOT). Symbols are looked up in the unit, then among the global symbols
   of the units loaded before, then in the process ([dlsym]). Since the
   units are not necessarily within reach of the process (2 GB on amd64,
   128 MB for the branches on arm64), the calls to external functions that
   are too far go through stubs (an indirect jump to the absolute address),
   and the addresses of external symbols are read from the GOT (the code is
   compiled with [-dlcode]: other references to external symbols are not
   emitted). *)

external page_size : unit -> int = "camljit_page_size"

external map : int -> int -> int = "camljit_map"

external write : int -> string -> unit = "camljit_write"

external protect : int -> int -> bool -> unit = "camljit_protect"

external dlsym : string -> int = "camljit_dlsym"

external self_address : unit -> int = "camljit_self_address"

external register : int -> int -> int -> int -> unit = "camljit_register"

external run : int -> unit = "camljit_run"

exception Error of string

let error fmt = Printf.ksprintf (fun msg -> raise (Error msg)) fmt

let () =
  Printexc.register_printer (function
    | Error msg -> Some msg
    | _ -> None)

(* Symbols are prefixed with [_] on macOS, but not in the names given to
   [dlsym]. *)
let symbol_prefix = if Config.system = "macosx" then "_" else ""

(* The global symbols of the units loaded. *)
let globals : (string, int) Hashtbl.t = Hashtbl.create 1024

let lookup_global (name : string) : int option =
  match Hashtbl.find_opt globals name with
  | Some address -> Some address
  | None ->
     let prefix_length = String.length symbol_prefix in
     if String.starts_with ~prefix:symbol_prefix name then
       let address =
         dlsym (String.sub name prefix_length
                  (String.length name - prefix_length)) in
       if address = 0 then None else Some address
     else
       None

(* [is_defined name] tells whether the symbol [name] (as named by
   [Compilenv], without prefix) is defined in the process or by a unit
   loaded before. *)
let is_defined (name : string) : bool =
  Option.is_some (lookup_global (symbol_prefix ^ name))

let align_up (n : int) (alignment : int) : int =
  (n + alignment - 1) / alignment * alignment

let fits_int32 (n : int) : bool = -0x8000_0000 <= n && n <= 0x7FFF_FFFF

let arm64 = Config.architecture = "arm64"

(* The stubs: [jmp *0(%rip); .quad target] on amd64,
   [ldr x16, 8; br x16; .quad target] on arm64 (x16 can be clobbered by the
   calls, as by the veneers of the linker). *)
let stub_size = if arm64 then 16 else 14

let stub_code =
  if arm64 then "\x50\x00\x00\x58\x00\x02\x1F\xD6"
  else "\xFF\x25\x00\x00\x00\x00"

(* Branches of 26 bits (in words) on arm64. *)
let fits_branch26 (n : int) : bool =
  n land 3 = 0 && - (1 lsl 27) <= n && n < 1 lsl 27

let page_of (address : int) : int = address land lnot 0xFFF

(* The address of the next mapping: units are placed after each other, from
   an address near the process if the system accepts it. *)
let next_address : int option ref = ref None

type unit_symbols = {
    frametable : int;
    gc_roots : int;
    code_begin : int;
    code_end : int;
    entry : int;
  }

(* [load unit_name obj] places [obj] in memory, relocates it and registers
   it in the runtime, and returns the address of its entry point (0 if
   none). *)
let load (unit_name : string) (obj : Object.t) : int =
  let page = page_size () in
  let local = Hashtbl.create 64 in
  List.iter (fun (name, symbol) -> Hashtbl.replace local name symbol) obj.symbols;
  let is_local name = Hashtbl.mem local name in
  (* Stubs for the external targets of calls, and GOT entries. *)
  let stubs = Hashtbl.create 16 in
  let got = Hashtbl.create 16 in
  Array.iter (fun (section : Object.section) ->
    List.iter (fun (r : Object.relocation) ->
      match r.kind with
      | (Branch32 | Arm64_branch26) when not (is_local r.target) ->
         if not (Hashtbl.mem stubs r.target) then
           Hashtbl.add stubs r.target (Hashtbl.length stubs)
      | Got_relative32 | Arm64_got_page21 | Arm64_got_pageoff12 ->
         if not (Hashtbl.mem got r.target) then
           Hashtbl.add got r.target (Hashtbl.length got)
      | _ -> ()) section.relocations) obj.sections;
  (* Layout. *)
  let offsets = Array.make (Array.length obj.sections) 0 in
  let place executable start =
    let offset = ref start in
    Array.iteri (fun i (section : Object.section) ->
      if section.executable = executable then
        begin
          offset := align_up !offset (max section.alignment 16);
          offsets.(i) <- !offset;
          offset := !offset + String.length section.contents
        end) obj.sections;
    !offset in
  let code_end = place true 0 in
  let stubs_offset = align_up code_end 16 in
  let code_size = align_up (stubs_offset + stub_size * Hashtbl.length stubs) page in
  let data_end = place false code_size in
  let got_offset = align_up data_end 8 in
  let size = max page (align_up (got_offset + 8 * Hashtbl.length got) page) in
  let hint =
    match !next_address with
    | Some address -> address
    | None -> align_up (self_address ()) page + (1 lsl 28) in
  let base = map hint size in
  next_address := Some (base + size);
  let image = Bytes.make size '\000' in
  Array.iteri (fun i (section : Object.section) ->
    Bytes.blit_string section.contents 0 image offsets.(i)
      (String.length section.contents)) obj.sections;
  (* Symbols. *)
  let address_of_local (symbol : Object.symbol) =
    base + offsets.(symbol.section) + symbol.position in
  let address name =
    match Hashtbl.find_opt local name with
    | Some symbol -> address_of_local symbol
    | None ->
       match lookup_global name with
       | Some address -> address
       | None -> error "JIT: undefined symbol %s (in %s)" name unit_name in
  Hashtbl.iter (fun name index ->
    let stub = stubs_offset + stub_size * index in
    let code_size = String.length stub_code in
    Bytes.blit_string stub_code 0 image stub code_size;
    Bytes.set_int64_le image (stub + code_size) (Int64.of_int (address name)))
    stubs;
  Hashtbl.iter (fun name index ->
    Bytes.set_int64_le image (got_offset + 8 * index)
      (Int64.of_int (address name))) got;
  (* Relocations. *)
  Array.iteri (fun i (section : Object.section) ->
    List.iter (fun (r : Object.relocation) ->
      let field = offsets.(i) + r.offset in
      let p = base + field in
      let addend = Int64.to_int r.addend in
      let set32 what n =
        if not (fits_int32 n) then
          error "JIT: %s to %s out of range (in %s)" what r.target unit_name;
        Bytes.set_int32_le image field (Int32.of_int n) in
      (* Fields of arm64 instructions. *)
      let patch mask value =
        let word = Int32.to_int (Bytes.get_int32_le image field) in
        Bytes.set_int32_le image field
          (Int32.of_int ((word land lnot mask) lor (value land mask))) in
      let set_page21 what target =
        let pages = (page_of target - page_of p) asr 12 in
        if pages < - (1 lsl 20) || pages >= 1 lsl 20 then
          error "JIT: %s to %s out of range (in %s)" what r.target unit_name;
        patch 0x60FFFFE0 (((pages land 3) lsl 29) lor (((pages asr 2) land 0x7FFFF) lsl 5)) in
      let set_pageoff12 what target shift =
        let offset = target land 0xFFF in
        if offset land ((1 lsl shift) - 1) <> 0 then
          error "JIT: %s to %s misaligned (in %s)" what r.target unit_name;
        patch 0x003FFC00 ((offset lsr shift) lsl 10) in
      let got_entry () = base + got_offset + 8 * Hashtbl.find got r.target in
      match r.kind with
      | Absolute64 ->
         Bytes.set_int64_le image field (Int64.of_int (address r.target + addend))
      | Absolute32_signed -> set32 "absolute address" (address r.target + addend)
      | Relative32 -> set32 "relative address" (address r.target + addend - p)
      | Branch32 ->
         let target = address r.target in
         let target =
           if fits_int32 (target + addend - p) then target
           else base + stubs_offset + stub_size * Hashtbl.find stubs r.target in
         set32 "branch" (target + addend - p)
      | Got_relative32 -> set32 "GOT entry" (got_entry () + addend - p)
      | Arm64_branch26 ->
         let target = address r.target in
         let target =
           if fits_branch26 (target + addend - p) then target
           else base + stubs_offset + stub_size * Hashtbl.find stubs r.target in
         let delta = target + addend - p in
         if not (fits_branch26 delta) then
           error "JIT: branch to %s out of range (in %s)" r.target unit_name;
         patch 0x03FFFFFF (delta asr 2)
      | Arm64_page21 -> set_page21 "page" (address r.target + addend)
      | Arm64_pageoff12 shift ->
         set_pageoff12 "page offset" (address r.target + addend) shift
      | Arm64_got_page21 -> set_page21 "GOT page" (got_entry ())
      | Arm64_got_pageoff12 -> set_pageoff12 "GOT page offset" (got_entry ()) 3)
      section.relocations)
    obj.sections;
  write base (Bytes.unsafe_to_string image);
  protect base code_size true;
  (* The global symbols are only made visible once the unit is loaded. *)
  List.iter (fun (name, (symbol : Object.symbol)) ->
    if symbol.global then Hashtbl.replace globals name (address_of_local symbol))
    obj.symbols;
  let find suffix =
    let name =
      symbol_prefix ^ Compilenv.make_symbol ~unitname:unit_name (Some suffix) in
    match Hashtbl.find_opt local name with
    | Some symbol -> address_of_local symbol
    | None -> 0 in
  let symbols = {
      frametable = find "frametable"; gc_roots = find "gc_roots";
      code_begin = find "code_begin"; code_end = find "code_end";
      entry = find "entry" } in
  register symbols.frametable symbols.gc_roots symbols.code_begin
    symbols.code_end;
  symbols.entry

let run_entry (entry : int) : unit =
  if entry <> 0 then run entry
