(* The pipeline of [ocamlopt] for units compiled in the current process, with
   the assembly of the code in memory ([Assembler]). *)

(* On Windows, the backend emits COFF assembly code, which neither the
   internal assemblers nor the loader support (and the primitives of the
   loader are not implemented, see camljit_stubs.c). *)
let available =
  Assembler.available && Sys.backend_type = Native && not Sys.win32

(* Stolen from native/topeval.ml *)
module Backend = struct
  (* See backend_intf.mli. *)

  let symbol_for_global' = Compilenv.symbol_for_global'
  let closure_symbol = Compilenv.closure_symbol

  let really_import_approx = Import_approx.really_import_approx
  let import_symbol = Import_approx.import_symbol

  let size_int = Sys.word_size
  let big_endian = Sys.big_endian

  let max_sensible_number_of_arguments =
    (* The "-1" is to allow for a potential closure environment parameter. *)
    Proc.max_arguments_for_tailcalls - 1
end

let backend = (module Backend : Backend_intf.S)

let with_value (r : 'a ref) (v : 'a) (f : unit -> 'b) : 'b =
  let saved = !r in
  r := v;
  Fun.protect f ~finally:(fun () -> r := saved)

let remove_without_failure (filename : string) : unit =
  try Sys.remove filename with Sys_error _ -> ()

let remove_directory (dir : string) : unit =
  try
    Array.iter (fun name -> remove_without_failure (Filename.concat dir name))
      (Sys.readdir dir);
    Sys.rmdir dir
  with Sys_error _ -> ()

let check_available () : unit =
  if not available then
    failwith "Camljit: no internal assembler for this architecture"

let lambda_program (module_name : string) (impl : Typedtree.implementation)
    : Lambda.program =
  let transl_implementation =
    if Config.flambda then Translmod.transl_implementation_flambda
    else Translmod.transl_store_implementation in
  let program =
    transl_implementation module_name (impl.structure, impl.coercion) in
  { program with code = Simplif.simplify_lambda program.code }

let compile_implementation ~(prefixname : string) (program : Lambda.program)
    : unit =
  let middle_end =
    if Config.flambda then Flambda_middle_end.lambda_to_clambda
    else Closure_middle_end.lambda_to_clambda in
  Asmgen.compile_implementation ~backend ~prefixname ~middle_end
    ~ppf_dump:Format.err_formatter program

let native ~(prefixname : string) (program : Lambda.program) : unit =
  compile_implementation ~prefixname program

(* With the internal assembler, the assembly code is not written to files
   (amd64: [X86_proc] passes the program to [Assembler]). *)
let with_internal_assembler (f : unit -> 'a) : 'a =
  with_value Emitaux.binary_backend_available true f

let in_memory ~(prefixname : string) (program : Lambda.program) : Object.t =
  check_available ();
  with_internal_assembler (fun () ->
    Assembler.assemble_in_memory ~output_prefix:prefixname (fun () ->
      compile_implementation ~prefixname program))

(* The generic functions are compiled in units of their own, numbered in the
   process. *)
let generic_units_counter = ref 0

let load_generic_functions (infos : Cmx_format.unit_infos list) : unit =
  check_available ();
  let phrases =
    Cmm_helpers.generic_functions true infos
    |> List.filter (fun (phrase : Cmm.phrase) ->
         match phrase with
         | Cfunction { fun_name; _ } -> not (Loader.is_defined fun_name)
         | Cdata _ -> true) in
  if phrases <> [] then
    begin
      incr generic_units_counter;
      let name =
        Printf.sprintf "Camljit_generic_%d" !generic_units_counter in
      let ppf_dump = Format.err_formatter in
      Compilenv.reset name;
      (* The prefix is unique, since the files of the unit can be written
         there (by [Assembler] on arm64). *)
      let prefix = Filename.temp_file name "" in
      let obj =
        Fun.protect ~finally:(fun () -> remove_without_failure prefix)
          (fun () ->
            with_internal_assembler (fun () ->
              Assembler.assemble_in_memory ~output_prefix:prefix (fun () ->
                Asmgen.compile_unit ~output_prefix:prefix
                  ~asm_filename:(prefix ^ ".s") ~keep_asm:false
                  ~obj_filename:(prefix ^ Config.ext_obj) (fun () ->
                    Emit.begin_assembly ();
                    List.iter (Asmgen.compile_phrase ~ppf_dump) phrases;
                    Emit.end_assembly ())))) in
      Loader.run_entry (Loader.load name obj)
    end

(* The units evaluated without name given are numbered in the process: a
   unit can only be loaded once (its symbols are global). *)
let eval_counter = ref 0

let fresh_module_name () : string =
  incr eval_counter;
  Printf.sprintf "Camljit_eval_%d" !eval_counter

let eval ?(module_name : string option) (env : Env.t)
    (structure : Parsetree.structure) : unit =
  check_available ();
  let module_name =
    match module_name with
    | Some name -> name
    | None -> fresh_module_name () in
  (* The files of the unit (its interface, written by the typer, and the
     temporary files of the assembler) are written in a temporary directory,
     named after the unit (as required for the interface). *)
  let dir = Filename.temp_dir "camljit" "" in
  Fun.protect ~finally:(fun () -> remove_directory dir) (fun () ->
    let prefixname = Filename.concat dir module_name in
    let unit_info = Unit_info.make ~source_file:"" Impl prefixname in
    Compilenv.reset module_name;
    let saved_unit = Env.get_current_unit () in
    Env.set_current_unit unit_info;
    let impl =
      Fun.protect (fun () ->
        Typemod.type_implementation unit_info env structure)
        ~finally:(fun () -> Option.iter Env.set_current_unit saved_unit) in
    let obj = in_memory ~prefixname (lambda_program module_name impl) in
    load_generic_functions [Compilenv.current_unit_infos ()];
    Loader.run_entry (Loader.load module_name obj))
