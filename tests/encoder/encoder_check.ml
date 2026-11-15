(* Compiles OCaml files with ocamlopt (same arguments), and checks that each
   section encoded by [X86_encoder] is identical to the one assembled by the
   external assembler from the same program (relocated fields are zero in
   both). Differences are reported with the disassembly of the section. *)

let failures = ref 0

let run (command : string) : unit =
  if Sys.command command <> 0 then failwith ("command failed: " ^ command)

let section_bytes (obj : string) (name : string) : string option =
  let out = Filename.temp_file "section" ".bin" in
  let ok =
    Sys.command
      (Printf.sprintf "objcopy -O binary --only-section=%s %s %s 2>/dev/null"
         (Filename.quote name) (Filename.quote obj) (Filename.quote out)) = 0 in
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
    (Printf.sprintf "objdump -D -b binary -m i386:x86-64 %s > %s"
       (Filename.quote bin) (Filename.quote txt)));
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
let object_relocations (obj : string) : (string * int * string * string * int64) list =
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

let kind_class (kind : string) : string =
  match kind with
  | "R_X86_64_PC32" | "R_X86_64_PLT32" -> "relative"
  | "R_X86_64_GOTPCREL" | "R_X86_64_GOTPCRELX" | "R_X86_64_REX_GOTPCRELX" -> "got"
  | "R_X86_64_64" -> "absolute64"
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

let check_relocations (outfile : string) (encoded : Camljit.Object.t) : unit =
  let expected =
    List.sort compare
      (List.map (fun (section, offset, kind, target, addend) ->
         (section, offset, kind_class kind, target, addend))
         (List.filter (fun (section, _, _, _, _) -> section <> ".eh_frame")
            (object_relocations outfile))) in
  let actual =
    Array.to_list encoded.sections |> List.concat_map (fun (section : Camljit.Object.section) ->
      List.map (fun (r : Camljit.Object.relocation) ->
        (* Local labels are referred to by their section in the object. *)
        let target, addend =
          match List.assoc_opt r.target encoded.symbols with
          | Some { global = false; section = index; position }
            when r.kind <> Got_relative32
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
  run (Printf.sprintf "as -o %s %s" (Filename.quote outfile) (Filename.quote asm));
  Sys.remove asm;
  let encoded = Camljit.X86_encoder.assemble program in
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
  check_relocations outfile encoded

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
  run (Printf.sprintf "as -o %s %s" (Filename.quote obj) (Filename.quote asm));
  let text = section_bytes obj ".text" in
  Sys.remove asm; Sys.remove obj;
  match text with
  | Some text when String.length text = 65 ->
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
