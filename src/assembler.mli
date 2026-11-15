(** Assembly in memory of the generated code, for the JIT. *)

val available : bool
(** [true] if the generated code can be assembled in memory (on amd64 and
    arm64). *)

val assemble_in_memory : output_prefix:string -> (unit -> unit) -> Object.t
(** [assemble_in_memory ~output_prefix f] runs [f], which compiles a unit
    with the given output prefix, and returns its machine code. *)
