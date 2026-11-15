(lang dune 3.0)

(generate_opam_files true)

(name camljit)
(license "BSD-2-Clause")
(maintainers "Thierry Martinez <thierry.martinez@inria.fr>")
(authors "Thierry Martinez <thierry.martinez@inria.fr>")
(source (uri "git+https://github.com/thierry-martinez/camljit.git"))
(homepage "https://github.com/thierry-martinez/camljit")
(bug_reports "https://github.com/thierry-martinez/camljit/issues")
(documentation "https://github.com/thierry-martinez/camljit")
(version "0.1")

(package
 (name camljit)
 (synopsis "In-memory assembler and loader for the OCaml native compiler")
 (description "\
camljit compiles OCaml code with compiler-libs and loads it in the
running native process without external assembler, linker or dynamic
loader: the assembly code produced by the backend of the compiler is
encoded in memory (amd64 and arm64), relocated, registered in the runtime
and run. It is used by ocip and metapp.
")
 (depends
  (ocaml (and (>= 5.3) (< 5.7)))
  (conf-binutils (and :with-test (= :os "linux")))))
