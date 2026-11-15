(** Compilation of OCaml code to machine code in memory, and evaluation in
    the current process.

    The pipeline is the one of [ocamlopt]: a typed implementation is
    translated to Lambda ({!lambda_program}), compiled by the native backend
    and assembled in memory ({!in_memory}), and the resulting {!Object.t} is
    loaded by {!Loader}. {!eval} chains all the steps for a parse tree.

    The generic functions that a shared library would contain
    ([caml_curryN], [caml_applyN], ...) are compiled on demand by
    {!load_generic_functions}.

    The process must be a native program, which exports the symbols of its
    units (OCaml links native executables with [-Wl,-E] on ELF systems, for
    natdynlink): the code loaded refers to them by name. *)

val available : bool
(** [true] if the code can be assembled in memory: on amd64 and arm64, in
    native code. Otherwise, the code has to be assembled and linked by
    external commands (for instance, with [ocamlopt -shared] and
    [Dynlink]). *)

val lambda_program : string -> Typedtree.implementation -> Lambda.program
(** [lambda_program module_name impl] translates and simplifies [impl] (with
    the translation of flambda when the compiler is configured with it). *)

val native : prefixname:string -> Lambda.program -> unit
(** [native ~prefixname program] compiles [program] (of the current unit of
    [Compilenv]) with the native backend, as [ocamlopt -c], with the
    external assembler: the object file is written to [prefixname ^ ".o"]
    (provided as an alternative to {!in_memory} for the programs that
    support both). *)

val in_memory : prefixname:string -> Lambda.program -> Object.t
(** [in_memory ~prefixname program] compiles [program] (of the current unit
    of [Compilenv], see [Compilenv.reset]) and assembles it in memory.
    Temporary files may be written with the prefix [prefixname] (on arm64).
    The information of the unit ([Compilenv.current_unit_infos], to save as
    a [.cmx] file) is up to date afterwards.
    @raise Failure if not {!available}. *)

val load_generic_functions : Cmx_format.unit_infos list -> unit
(** [load_generic_functions infos] compiles and loads the generic functions
    that the units described by [infos] need and that the process (or a
    unit loaded before) does not define yet. To call before loading the
    units, since they refer to these functions. [Compilenv] is reset. *)

val eval : ?module_name:string -> Env.t -> Parsetree.structure -> unit
(** [eval env structure] types [structure] in [env], compiles it into a new
    unit, loads it and runs its initialization code (the top-level
    definitions of the unit). The name of the unit is [module_name] if
    given (it must not be the name of a unit already loaded), and a fresh
    name otherwise. Errors of typing are raised as by the compiler (to
    report with [Location.report_exception]), the errors of loading as
    {!Loader.Error}.
    @raise Failure if not {!available}. *)
