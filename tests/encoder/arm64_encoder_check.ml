(* Compiles OCaml files with ocamlopt (same arguments, which have to include
   [-S] to keep the assembly code), and checks that each section encoded by
   [Arm64_encoder] from the assembly code is identical to the one assembled
   by the external assembler (on macOS, the object files are read as
   Mach-O; on Linux, as ELF with GNU binutils), except for the relocated
   fields. The relocations are compared too: their external targets by name,
   and their targets in the unit by address (in the layout of the object
   file) for the absolute addresses on macOS, by section and offset on
   Linux. Differences are reported with the words that differ. *)

let failures = ref 0

let u32 (s : string) (o : int) : int =
  Int32.to_int (String.get_int32_le s o) land 0xFFFF_FFFF

let u64 (s : string) (o : int) : int = Int64.to_int (String.get_int64_le s o)

let c_string (s : string) (o : int) (max : int) : string =
  let rec stop i = if i < o + max && i < String.length s && s.[i] <> '\000'
                   then stop (i + 1) else i in
  String.sub s o (stop o - o)

type macho_section = {
    name : string;
    address : int;
    contents : string;
    relocations : (int * string * string) list;
      (** Offset, kind and target: the name of an external symbol,
          [@address] for an absolute address in the object, or [defined]. *)
  }

(* The sections of a Mach-O object file (64 bits), with their relocations. *)
let read_macho (obj : string) : macho_section list =
  let s = In_channel.with_open_bin obj In_channel.input_all in
  if u32 s 0 <> 0xFEEDFACF then failwith (obj ^ ": not a Mach-O 64-bit file");
  let ncmds = u32 s 16 in
  let sections = ref [] and symbols = ref [||] in
  let p = ref 32 in
  for _ = 1 to ncmds do
    let cmd = u32 s !p and cmdsize = u32 s (!p + 4) in
    if cmd = 0x19 (* LC_SEGMENT_64 *) then
      for i = 0 to u32 s (!p + 64) - 1 do
        let q = !p + 72 + 80 * i in
        sections := (c_string s q 16, c_string s (q + 16) 16, u64 s (q + 32),
                     u64 s (q + 40), u32 s (q + 48), u32 s (q + 56),
                     u32 s (q + 60))
                    :: !sections
      done
    else if cmd = 0x2 (* LC_SYMTAB *) then
      begin
        let symoff = u32 s (!p + 8) and nsyms = u32 s (!p + 12)
        and stroff = u32 s (!p + 16) in
        (* Name, whether the symbol is defined (N_SECT), and value. *)
        symbols := Array.init nsyms (fun i ->
          let n = symoff + 16 * i in
          (c_string s (stroff + u32 s n) (String.length s),
           Char.code s.[n + 4] land 0x0E = 0x0E, u64 s (n + 8)))
      end;
    p := !p + cmdsize
  done;
  List.rev_map (fun (sectname, segname, address, size, offset, reloff, nreloc) ->
    let relocations =
      List.init nreloc (fun i ->
        let address = u32 s (reloff + 8 * i)
        and info = u32 s (reloff + 8 * i + 4) in
        let symbolnum = info land 0xFFFFFF
        and length = (info lsr 25) land 3
        and extern = (info lsr 27) land 1 = 1
        and kind = info lsr 28 in
        let kind =
          match kind, length with
          | 0, 3 -> "absolute64" | 0, 2 -> "absolute32" | 1, _ -> "subtractor"
          | 2, _ -> "branch26" | 3, _ -> "page21" | 4, _ -> "pageoff12"
          | 5, _ -> "got_page21" | 6, _ -> "got_pageoff12" | 10, _ -> "addend"
          | k, _ -> Printf.sprintf "type%d" k in
        let field () = if kind = "absolute64" then u64 s (offset + address) else 0 in
        let target =
          match extern with
          | true ->
             let name, defined, value = !symbols.(symbolnum) in
             if not defined then name
             else if kind = "absolute64" then Printf.sprintf "@%x" (value + field ())
             else "defined"
          | false ->
             if kind = "absolute64" then Printf.sprintf "@%x" (field ())
             else "defined" in
        (address, kind, target))
      |> List.filter (fun (_, kind, _) -> kind <> "addend") in
    let contents =
      if offset = 0 then String.make size '\000' else String.sub s offset size in
    { name = segname ^ "," ^ sectname; address; contents; relocations })
    !sections

let run (command : string) : unit =
  if Sys.command command <> 0 then failwith ("command failed: " ^ command)

(* The sections of an ELF object file, read with GNU binutils (objcopy,
   readelf), with their relocations. The targets of the relocations are the
   names of the symbols, or [section+offset] for the local labels (which the
   assembler refers to by their section, except in the GOT relocations). *)
let read_elf (obj : string) : macho_section list =
  let txt = Filename.temp_file "elf" ".txt" in
  run (Printf.sprintf "readelf -SW %s > %s" (Filename.quote obj) (Filename.quote txt));
  (* Index, name, and whether the section has contents. *)
  let all_sections =
    In_channel.with_open_text txt In_channel.input_lines
    |> List.filter_map (fun line ->
         let line = String.trim line in
         match String.index_opt line ']' with
         | Some i when String.starts_with ~prefix:"[" line ->
            let index = int_of_string_opt (String.trim (String.sub line 1 (i - 1))) in
            begin match index, String.split_on_char ' '
                                 (String.sub line (i + 1) (String.length line - i - 1))
                               |> List.filter ((<>) "") with
            | Some index, name :: kind :: _ ->
               Some (index, name,
                     not (List.mem kind ["NULL"; "RELA"; "SYMTAB"; "STRTAB"]))
            | _ -> None
            end
         | _ -> None) in
  let names =
    List.filter_map (fun (_, name, keep) -> if keep then Some name else None)
      all_sections in
  (* The local symbols kept by the assembler (for instance, the labels with a
     [.type]), as their section and offset. *)
  run (Printf.sprintf "readelf -sW %s > %s" (Filename.quote obj) (Filename.quote txt));
  let locals = Hashtbl.create 64 in
  In_channel.with_open_text txt In_channel.input_lines
  |> List.iter (fun line ->
       match String.split_on_char ' ' line |> List.filter ((<>) "") with
       | [_num; value; _size; _type; "LOCAL"; _vis; ndx; name] ->
          begin match int_of_string_opt ndx with
          | Some ndx ->
             List.iter (fun (index, section, _) ->
               if index = ndx then
                 Hashtbl.replace locals name
                   (section, int_of_string ("0x" ^ value))) all_sections
          | None -> ()
          end
       | _ -> ());
  run (Printf.sprintf "readelf -rW %s > %s" (Filename.quote obj) (Filename.quote txt));
  let lines = In_channel.with_open_text txt In_channel.input_lines in
  Sys.remove txt;
  let kind_class kind =
    match kind with
    | "R_AARCH64_ABS64" -> "absolute64"
    | "R_AARCH64_ABS32" -> "absolute32"
    | "R_AARCH64_PREL32" -> "relative32"
    | "R_AARCH64_CALL26" | "R_AARCH64_JUMP26" -> "branch26"
    | "R_AARCH64_ADR_PREL_PG_HI21" -> "page21"
    | "R_AARCH64_ADD_ABS_LO12_NC" | "R_AARCH64_LDST8_ABS_LO12_NC"
    | "R_AARCH64_LDST16_ABS_LO12_NC" | "R_AARCH64_LDST32_ABS_LO12_NC"
    | "R_AARCH64_LDST64_ABS_LO12_NC" | "R_AARCH64_LDST128_ABS_LO12_NC" ->
       "pageoff12"
    | "R_AARCH64_ADR_GOT_PAGE" -> "got_page21"
    | "R_AARCH64_LD64_GOT_LO12_NC" -> "got_pageoff12"
    | k -> k in
  let normalize kind target addend =
    match Hashtbl.find_opt locals target with
    | _ when List.mem target names -> Printf.sprintf "%s+%x" target addend
    | Some (section, value) when kind <> "got_page21" && kind <> "got_pageoff12" ->
       Printf.sprintf "%s+%x" section (value + addend)
    | _ -> if addend = 0 then target else Printf.sprintf "%s%+d" target addend in
  let section = ref "" in
  let relocations =
    List.filter_map (fun line ->
      match String.split_on_char ' ' line |> List.filter ((<>) "") with
      | "Relocation" :: "section" :: name :: _ ->
         let name = String.sub name 1 (String.length name - 2) in
         section := (if String.starts_with ~prefix:".rela" name then
                       String.sub name 5 (String.length name - 5) else name);
         None
      | offset :: _info :: kind :: _value :: target :: sign :: addend :: _
        when String.length offset = 16 ->
         let addend = int_of_string ("0x" ^ addend) in
         let addend = if sign = "-" then - addend else addend in
         Some (!section,
               (int_of_string ("0x" ^ offset), kind_class kind,
                normalize (kind_class kind) target addend))
      | [offset; _info; kind; _value; target] when String.length offset = 16 ->
         Some (!section,
               (int_of_string ("0x" ^ offset), kind_class kind,
                normalize (kind_class kind) target 0))
      | _ -> None) lines in
  List.map (fun name ->
    let out = Filename.temp_file "section" ".bin" in
    run (Printf.sprintf "objcopy -O binary --only-section=%s %s %s"
           (Filename.quote name) (Filename.quote obj) (Filename.quote out));
    let contents = In_channel.with_open_bin out In_channel.input_all in
    Sys.remove out;
    { name; address = 0; contents;
      relocations =
        List.filter_map (fun (s, r) -> if s = name then Some r else None)
          relocations })
    names

let elf = Ocaml_common.Config.system <> "macosx"

let encoder_kind (kind : Camljit.Object.relocation_kind) : string =
  match kind with
  | Absolute64 -> "absolute64"
  | Absolute32_signed -> "absolute32"
  | Relative32 -> "relative32"
  | Branch32 -> "branch32"
  | Got_relative32 -> "got_relative32"
  | Arm64_branch26 -> "branch26"
  | Arm64_page21 -> "page21"
  | Arm64_pageoff12 _ -> "pageoff12"
  | Arm64_got_page21 -> "got_page21"
  | Arm64_got_pageoff12 -> "got_pageoff12"

(* The bits of the field of a relocation. *)
let mask (kind : string) : int * int =
  match kind with
  | "absolute64" -> 8, -1
  | "absolute32" | "relative32" -> 4, 0xFFFF_FFFF
  | "branch26" -> 4, 0x03FF_FFFF
  | "page21" | "got_page21" -> 4, 0x60FF_FFE0
  | "pageoff12" | "got_pageoff12" -> 4, 0x003F_FC00
  | _ -> 4, 0xFFFF_FFFF

let clear_fields (contents : string) (relocations : (int * string * string) list)
    : string =
  let b = Bytes.of_string contents in
  List.iter (fun (offset, kind, _) ->
    match mask kind with
    | 8, _ -> Bytes.set_int64_le b offset 0L
    | _, m ->
       let word = Int32.to_int (Bytes.get_int32_le b offset) land 0xFFFF_FFFF in
       Bytes.set_int32_le b offset (Int32.of_int (word land lnot m))) relocations;
  Bytes.to_string b

let words_around (contents : string) (offset : int) : string =
  let start = max 0 (offset / 4 * 4 - 16) in
  let stop = min (String.length contents) (offset / 4 * 4 + 20) in
  let rec loop o acc =
    if o + 4 > stop then List.rev acc
    else loop (o + 4) (Printf.sprintf "  %06x: %08x" o (u32 contents o) :: acc) in
  String.concat "\n" (loop start [])

let check (asm : string) (obj : string) : unit =
  let failures_before = !failures in
  let encoded =
    Camljit.Arm64_encoder.assemble (In_channel.with_open_bin asm In_channel.input_all) in
  let expected = if elf then read_elf obj else read_macho obj in
  (* The address of a section of the encoder in the object file. *)
  let address index =
    let name = encoded.sections.(index).name in
    match List.find_opt (fun (s : macho_section) -> s.name = name) expected with
    | Some s -> s.address
    | None -> 0 in
  Array.iter (fun (section : Camljit.Object.section) ->
    let actual_relocations =
      List.map (fun (r : Camljit.Object.relocation) ->
        let target =
          match List.assoc_opt r.target encoded.symbols, r.kind with
          | _, (Arm64_got_page21 | Arm64_got_pageoff12) when elf -> r.target
          | Some { global = false; section; position }, _ when elf ->
             Printf.sprintf "%s+%x" encoded.sections.(section).name
               (position + Int64.to_int r.addend)
          | _ when elf ->
             if r.addend = 0L then r.target
             else Printf.sprintf "%s%+Ld" r.target r.addend
          | Some symbol, Absolute64 ->
             Printf.sprintf "@%x"
               (address symbol.section + symbol.position + Int64.to_int r.addend)
          | Some _, _ -> "defined"
          | None, _ -> r.target in
        (r.offset, encoder_kind r.kind, target)) section.relocations
      |> List.sort compare in
    match List.find_opt (fun (s : macho_section) -> s.name = section.name) expected with
    | None ->
       if section.contents <> "" then
         begin
           incr failures;
           Printf.printf "%s: section %s missing in the object\n%!" obj section.name
         end
    | Some s ->
       (* On ELF, the assembler keeps the branches to the global symbols of
          the section, which the encoder resolves. *)
       let resolved (_, kind, target) =
         elf && kind = "branch26"
         && match List.assoc_opt target encoded.symbols with
            | Some symbol -> encoded.sections.(symbol.section).name = section.name
            | None -> false in
       let expected_relocations =
         List.sort compare (List.filter (fun r -> not (resolved r)) s.relocations) in
       let all = s.relocations @ actual_relocations in
       let e = clear_fields s.contents all
       and a = clear_fields section.contents all in
       if e <> a then
         begin
           incr failures;
           let n = min (String.length e) (String.length a) in
           let rec first i = if i < n && e.[i] = a.[i] then first (i + 1) else i in
           let i = first 0 in
           Printf.printf
             "%s: section %s differs at offset 0x%x (sizes: as %d, encoder %d)\n\
              --- as:\n%s\n--- encoder:\n%s\n%!"
             asm section.name i (String.length e) (String.length a)
             (words_around e i) (words_around a i)
         end;
       if expected_relocations <> actual_relocations then
         begin
           incr failures;
           let show (offset, kind, target) =
             Printf.sprintf "%s+0x%x %s %s" section.name offset kind target in
           let only_in a b = List.filter (fun r -> not (List.mem r b)) a in
           let first l = List.filteri (fun i _ -> i < 10) l in
           Printf.printf "%s: relocations differ\n  only in as:\n    %s\n  only in encoder:\n    %s\n%!"
             asm
             (String.concat "\n    "
                (List.map show (first (only_in expected_relocations actual_relocations))))
             (String.concat "\n    "
                (List.map show (first (only_in actual_relocations expected_relocations))))
         end) encoded.sections;
  (* What was compared, in the log of the test. *)
  if !failures = failures_before then
    Printf.printf "%s: %d sections (%d bytes) and %d relocations identical \
                   to the assembler\n%!"
      asm (Array.length encoded.sections)
      (Array.fold_left (fun n (s : Camljit.Object.section) ->
         n + String.length s.contents) 0 encoded.sections)
      (Array.fold_left (fun n (s : Camljit.Object.section) ->
         n + List.length s.relocations) 0 encoded.sections)

let () =
  let status = Optmaindriver.main Sys.argv Format.err_formatter in
  if status = 0 then
    Array.iter (fun arg ->
      if Filename.check_suffix arg ".ml" then
        let base = Filename.chop_suffix arg ".ml" in
        try check (base ^ ".s") (base ^ ".o")
        with Camljit.Arm64_encoder.Unsupported msg ->
          incr failures;
          Printf.printf "%s.s: unsupported: %s\n%!" base msg) Sys.argv;
  if !failures > 0 then exit 1;
  exit status
