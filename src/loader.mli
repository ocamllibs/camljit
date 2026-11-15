(** Loading of machine code ({!Object.t}) in the memory of the process. *)

exception Error of string
(** Raised when a unit cannot be loaded: undefined symbol, relocation out
    of range. *)

val is_defined : string -> bool
(** [is_defined name] tells whether the symbol [name] (as named by
    [Compilenv], without the prefix of the platform) is defined in the
    process or by a unit loaded before. *)

val load : string -> Object.t -> int
(** [load unit_name obj] places [obj] in memory, relocates it, registers it
    in the runtime (frame table, global roots, code fragment), and makes its
    global symbols visible to the units loaded afterwards. Returns the
    address of the entry point of the unit (its initialization code), 0 if
    none: the initialization code is not run (see {!run_entry}), so that
    the units that refer to each other can be loaded first, as by the
    dynamic linker.
    @raise Error if a symbol is undefined or out of range. *)

val run_entry : int -> unit
(** [run_entry entry] runs the initialization code returned by {!load}
    (nothing if [entry] is 0). The exceptions it raises are propagated. *)
