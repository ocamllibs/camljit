(* Compiles OCaml files with ocamlopt (same arguments), and checks that each
   section encoded by [X86_encoder] is identical to the one assembled by the
   external assembler from the same program (relocated fields are zero in
   both). Differences are reported with the disassembly of the section.

   On Linux, the object files are ELF (GNU binutils); on Windows (mingw-w64),
   they are COFF, read with the binutils of mingw-w64 (prefixed with
   [x86_64-w64-mingw32-]), where the addends of the relocations are stored
   in the relocated fields: they are read and cleared before the
   comparison. *)

let failures = ref 0

let coff = Config.system = "mingw64"

let tool (name : string) : string =
  if coff then "x86_64-w64-mingw32-" ^ name else name

let null_device = if Sys.win32 then "NUL" else "/dev/null"

let run (command : string) : unit =
  if Sys.command command <> 0 then failwith ("command failed: " ^ command)

let section_bytes (obj : string) (name : string) : string option =
  let out = Filename.temp_file "section" ".bin" in
  let ok =
    Sys.command
      (Printf.sprintf "%s -O binary --only-section=%s %s %s 2>%s"
         (tool "objcopy") (Filename.quote name) (Filename.quote obj)
         (Filename.quote out) null_device) = 0 in
  let contents =
    if ok then Some (In_channel.with_open_bin out In_channel.input_all)
    else None in
  Sys.remove out;
  contents

let disassemble (bytes : string) : string =
  let bin = Filename.temp_file "encoded" ".bin" in
  let txt = Filename.temp_file "encoded" ".txt" in
  Out_channel.with_open_bin bin (fun oc -> output_string oc bytes);
  ignore (Sys.command
    (Printf.sprintf "%s -D -b binary -m i386:x86-64 %s > %s"
       (tool "objdump") (Filename.quote bin) (Filename.quote txt)));
  let s = In_channel.with_open_bin txt In_channel.input_all in
  Sys.remove bin; Sys.remove txt;
  s

let lines_around (text : string) (offset : int) : string =
  let lines = String.split_on_char '\n' text in
  let address l =
    match String.index_opt l ':' with
    | Some i -> int_of_string_opt ("0x" ^ String.trim (String.sub l 0 i))
    | None -> None in
  let selected =
    List.filter (fun l ->
      match address l with
      | Some a -> a >= offset - 24 && a <= offset + 16
      | None -> false) lines in
  String.concat "\n" selected

(* The relocations of the object file, as (section, offset, kind, target,
   addend), where the target is a symbol name or a section name (for local
   labels). *)
let elf_relocations (obj : string) : (string * int * string * string * int64) list =
  let txt = Filename.temp_file "relocations" ".txt" in
  run (Printf.sprintf "readelf -rW %s > %s" (Filename.quote obj) (Filename.quote txt));
  let lines = In_channel.with_open_text txt In_channel.input_lines in
  Sys.remove txt;
  let section = ref "" in
  List.filter_map (fun line ->
    match String.split_on_char ' ' line |> List.filter ((<>) "") with
    | "Relocation" :: "section" :: name :: _ ->
       (* 'Relocation section '.rela.text' at offset ... *)
       let name = String.sub name 1 (String.length name - 2) in
       section := (if String.starts_with ~prefix:".rela" name then
                     String.sub name 5 (String.length name - 5) else name);
       None
    | offset :: _info :: kind :: _value :: target :: sign :: addend :: _
      when String.length offset = 16 ->
       let addend = Int64.of_string ("0x" ^ addend) in
       let addend = if sign = "-" then Int64.neg addend else addend in
       Some (!section, int_of_string ("0x" ^ offset), kind, target, addend)
    | [offset; _info; kind; _value; target] when String.length offset = 16 ->
       Some (!section, int_of_string ("0x" ^ offset), kind, target, 0L)
    | _ -> None) lines

(* The relocations of a COFF object file, with their addends read from the
   relocated fields of the sections ([sections]: the contents of the
   sections by name, whose relocated fields are cleared). The addends are
   converted to those of [Camljit.Object]: a 32-bit relative field is
   relative to its end in COFF, to its address in [Camljit.Object]. *)
let coff_relocations (obj : string) (sections : (string, Bytes.t) Hashtbl.t)
    : (string * int * string * string * int64) list =
  let txt = Filename.temp_file "relocations" ".txt" in
  run (Printf.sprintf "%s -r %s > %s" (tool "objdump") (Filename.quote obj)
         (Filename.quote txt));
  let lines = In_channel.with_open_text txt In_channel.input_lines in
  Sys.remove txt;
  let section = ref "" in
  List.filter_map (fun line ->
    match String.split_on_char ' ' line |> List.filter ((<>) "") with
    | ["RELOCATION"; "RECORDS"; "FOR"; name] ->
       (* 'RELOCATION RECORDS FOR [.text]:' *)
       section := String.sub name 1 (String.length name - 3);
       None
    | [offset; kind; target] when String.length offset = 16 ->
       let offset = int_of_string ("0x" ^ offset) in
       let contents =
         match Hashtbl.find_opt sections !section with
         | Some contents -> contents
         | None ->
            let contents =
              match section_bytes obj !section with
              | Some s -> Bytes.of_string s
              | None -> failwith ("cannot read the section " ^ !section) in
            Hashtbl.add sections !section contents;
            contents in
       let addend =
         match kind with
         | "IMAGE_REL_AMD64_ADDR64" ->
            let a = Bytes.get_int64_le contents offset in
            Bytes.set_int64_le contents offset 0L;
            a
         | _ ->
            let a = Int64.of_int32 (Bytes.get_int32_le contents offset) in
            Bytes.set_int32_le contents offset 0l;
            if kind = "IMAGE_REL_AMD64_REL32" then Int64.sub a 4L else a in
       Some (!section, offset, kind, target, addend)
    | _ -> None) lines

let kind_class (kind : string) : string =
  match kind with
  | "R_X86_64_PC32" | "R_X86_64_PLT32" | "IMAGE_REL_AMD64_REL32" -> "relative"
  | "R_X86_64_GOTPCREL" | "R_X86_64_GOTPCRELX" | "R_X86_64_REX_GOTPCRELX" -> "got"
  | "R_X86_64_64" | "IMAGE_REL_AMD64_ADDR64" -> "absolute64"
  | "R_X86_64_32S" -> "absolute32"
  | k -> k

let encoder_kind_class (kind : Camljit.Object.relocation_kind) : string =
  match kind with
  | Relative32 | Branch32 -> "relative"
  | Got_relative32 -> "got"
  | Absolute64 -> "absolute64"
  | Absolute32_signed -> "absolute32"
  | Arm64_branch26 | Arm64_page21 | Arm64_got_page21 | Arm64_got_pageoff12
  | Arm64_pageoff12 _ ->
     (* Never produced by the amd64 encoder: reported as a difference. *)
     "aarch64"

let check_relocations
    (relocations : (string * int * string * string * int64) list)
    (outfile : string) (encoded : Camljit.Object.t) : unit =
  let expected =
    List.sort compare
      (List.map (fun (section, offset, kind, target, addend) ->
         (section, offset, kind_class kind, target, addend))
         (List.filter (fun (section, _, _, _, _) -> section <> ".eh_frame")
            relocations)) in
  let actual =
    Array.to_list encoded.sections |> List.concat_map (fun (section : Camljit.Object.section) ->
      List.map (fun (r : Camljit.Object.relocation) ->
        (* Local labels are referred to by their section in the object, as
           well as global symbols in COFF (where they cannot be
           preempted). *)
        let target, addend =
          match List.assoc_opt r.target encoded.symbols with
          | Some { global; section = index; position }
            when (not global || coff) && r.kind <> Got_relative32
                 (* Labels of mergeable sections are kept. *)
                 && not (String.starts_with ~prefix:".rodata.cst"
                           encoded.sections.(index).name) ->
             encoded.sections.(index).name,
             Int64.add r.addend (Int64.of_int position)
          | _ -> r.target, r.addend in
        (section.name, r.offset, encoder_kind_class r.kind, target, addend))
        section.relocations)
    |> List.sort compare in
  if expected <> actual then
    begin
      incr failures;
      let show (section, offset, kind, target, addend) =
        Printf.sprintf "%s+0x%x %s %s%+Ld" section offset kind target addend in
      let only_in a b = List.filter (fun r -> not (List.mem r b)) a in
      Printf.printf "%s: relocations differ\n  only in as:\n    %s\n  only in encoder:\n    %s\n%!"
        outfile
        (String.concat "\n    " (List.map show (List.filteri (fun i _ -> i < 10) (only_in expected actual))))
        (String.concat "\n    " (List.map show (List.filteri (fun i _ -> i < 10) (only_in actual expected))))
    end

let check (program : X86_ast.asm_program) (outfile : string) : unit =
  let asm = Filename.chop_extension outfile ^ ".check.s" in
  Out_channel.with_open_text asm (fun oc -> X86_gas.generate_asm oc program);
  run (Printf.sprintf "%s -o %s %s" (tool "as") (Filename.quote outfile)
         (Filename.quote asm));
  Sys.remove asm;
  let encoded = Camljit.X86_encoder.assemble program in
  (* The sections whose relocated fields are cleared (COFF). *)
  let sections = Hashtbl.create 8 in
  let relocations =
    if coff then coff_relocations outfile sections
    else elf_relocations outfile in
  let section_bytes outfile name =
    match Hashtbl.find_opt sections name with
    | Some contents -> Some (Bytes.to_string contents)
    | None -> section_bytes outfile name in
  (* The assembler pads the COFF sections to their alignment, with
     one-byte no-operations in code and zeros in data. *)
  let strip_padding (section : Camljit.Object.section) (expected : string) =
    let n = String.length section.contents in
    let padding = if section.executable then '\x90' else '\x00' in
    if coff && String.length expected > n
       && String.starts_with ~prefix:section.contents expected
       && String.for_all (( = ) padding)
            (String.sub expected n (String.length expected - n)) then
      section.contents
    else expected in
  Array.iter (fun (section : Camljit.Object.section) ->
    match section_bytes outfile section.name with
    | None ->
       if section.contents <> "" then
         begin
           incr failures;
           Printf.printf "%s: section %s missing in the object\n" outfile
             section.name
         end
    | Some expected ->
       let expected = strip_padding section expected in
       if expected <> section.contents then
         begin
           incr failures;
           let n = min (String.length expected) (String.length section.contents) in
           let rec first i =
             if i < n && expected.[i] = section.contents.[i] then first (i + 1)
             else i in
           let i = first 0 in
           Printf.printf
             "%s: section %s differs at offset 0x%x (sizes: as %d, encoder %d)\n\
              --- as:\n%s\n--- encoder:\n%s\n%!"
             outfile section.name i (String.length expected)
             (String.length section.contents)
             (lines_around (disassemble expected) i)
             (lines_around (disassemble section.contents) i)
         end) encoded.sections;
  check_relocations relocations outfile encoded

(* The order of the no-operations with which the external assembler pads
   code, which depends on its version (see [X86_encoder.padding_order]): a
   gap of 12 bytes is padded with a no-operation of 11 bytes and one of 1
   byte. *)
let assembler_padding_order () : Camljit.X86_encoder.padding_order =
  let asm = Filename.temp_file "padding" ".s" in
  let obj = Filename.temp_file "padding" ".o" in
  Out_channel.with_open_text asm (fun oc ->
    for _ = 1 to 52 do output_string oc "push %rax\n" done;
    output_string oc ".p2align 6\npush %rax\n");
  run (Printf.sprintf "%s -o %s %s" (tool "as") (Filename.quote obj)
         (Filename.quote asm));
  let text = section_bytes obj ".text" in
  Sys.remove asm; Sys.remove obj;
  match text with
  (* The COFF sections are padded to their alignment (64 here). *)
  | Some text
    when String.length text = 65 || coff && String.length text = 128 ->
     let pad = String.sub text 52 12 in
     if pad = Camljit.X86_encoder.(nops.(11) ^ nops.(1)) then Longest_first
     else if pad = Camljit.X86_encoder.(nops.(1) ^ nops.(11)) then
       Remainder_first
     else failwith "unknown padding of the external assembler"
  | _ -> failwith "cannot read the padding of the external assembler"

let () =
  Camljit.X86_encoder.padding_order := assembler_padding_order ();
  X86_proc.register_internal_assembler (fun program outfile ->
    try check program outfile
    with Camljit.X86_encoder.Unsupported msg ->
      incr failures;
      Printf.printf "%s: unsupported: %s\n%!" outfile msg);
  let status = Optmaindriver.main Sys.argv Format.err_formatter in
  if !failures > 0 then exit 1;
  exit status
