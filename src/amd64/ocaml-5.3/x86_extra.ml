(* The instructions of [X86_ast] that only exist in some versions of OCaml
   (see [X86_encoder.encode_extra]): before OCaml 5.6, there is none. The
   type [t] is the same in [ocaml-5.6/x86_extra.ml]. *)

type t =
  | Bsf of X86_ast.arg * X86_ast.arg
  | Bsr of X86_ast.arg * X86_ast.arg
  | Bts of X86_ast.arg * X86_ast.arg
  | Xadd of X86_ast.arg * X86_ast.arg
  | Lock_xadd of X86_ast.arg * X86_ast.arg

let classify (_ : X86_ast.instruction) : t option = None
