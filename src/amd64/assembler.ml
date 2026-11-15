(* Assembly in memory, on amd64 (see [Compile]): the internal assembler of
   the compiler ([X86_proc]) receives the program, encoded by [X86_encoder]. *)

let available = true

let assemble_in_memory ~output_prefix:_ (f : unit -> unit) : Object.t =
  let program = ref None in
  X86_proc.with_internal_assembler
    (fun asm -> program := Some asm; fun _obj_filename -> ()) f;
  match !program with
  | Some program -> X86_encoder.assemble program
  | None -> failwith "assemble_in_memory: no code generated"
