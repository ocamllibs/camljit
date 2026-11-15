# camljit

camljit compiles OCaml code with `compiler-libs` and loads it in the running
native process, without external assembler, linker or dynamic loader:

- the assembly code produced by the native backend is encoded in memory by
  an internal assembler (`X86_encoder` on amd64, `Arm64_encoder` on arm64),
  which follows the encoding choices of the GNU assembler;
- the machine code (`Camljit.Object.t`: sections, symbols and relocations)
  is placed in memory, relocated, registered in the runtime (frame table,
  global roots, code fragment) and run by `Camljit.Loader`;
- the generic functions that a shared library would contain
  (`caml_curryN`, `caml_applyN`, ...) are compiled on demand.

Loading a unit this way takes a fraction of the time of `ocamlopt -shared`
followed by `Dynlink.loadfile`, which runs the assembler and the linker.

camljit was extracted from [ocip](https://github.com/thierry-martinez/ocip)
(OCaml in Python), which compiles conversion code at runtime, and is also
used by [metapp](https://github.com/thierry-martinez/metapp) to evaluate the
`[%meta ...]` code of the preprocessor.

## Usage

```ocaml
let () =
  Clflags.include_dirs := [ (* directories of the .cmi used by the code *) ];
  Compmisc.init_path ();
  let env = Compmisc.initial_env () in
  Camljit.Compile.eval env
    (Parse.implementation (Lexing.from_string {| let () = print_endline "hello" |}))
```

`Camljit.Compile.available` tells whether the internal assembler exists for
the architecture (amd64 and arm64, in native code): otherwise, fall back to
`ocamlopt -shared` and `Dynlink`.

The lower-level functions let a program keep the compiled objects (for
instance, in a cache) and load several units together:
`Compile.lambda_program`, `Compile.in_memory`,
`Compile.load_generic_functions`, `Loader.load` and `Loader.run_entry`.

The code loaded refers to the symbols of the process by name (`dlsym`):
the program must export them, as OCaml does for native executables on ELF
systems (`-Wl,-E`, used for natdynlink) and for shared libraries.

## Supported platforms

OCaml 5.3 to 5.6, on Linux and macOS, amd64 and arm64 (other architectures
build, with `Compile.available = false`). Not available on Windows.

## Tests

`dune runtest` evaluates code with `Compile.eval` (`tests/eval`), and checks
that the internal assemblers encode the same machine code and relocations as
the external assembler on modules of the standard library
(`tests/encoder`: GNU binutils on Linux, Mach-O object files on macOS).

The GitLab CI (`.gitlab-ci.yml`) runs them on Linux amd64; the GitHub
workflow (`.github/workflows/main.yml`) on Linux and macOS, amd64 and
arm64, with OCaml 5.3 to 5.5 and the development version 5.6.
