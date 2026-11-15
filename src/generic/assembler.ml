(* Assembly in memory is only implemented on amd64 and arm64 (see
   [amd64/assembler.ml] and [arm64/assembler.ml]): on other architectures,
   the users of camljit assemble and link the code with external commands
   (see [Camljit.available]). *)

let available = false

let assemble_in_memory ~output_prefix:_ (_ : unit -> unit) : Object.t =
  failwith "assemble_in_memory: no internal assembler for this architecture"
