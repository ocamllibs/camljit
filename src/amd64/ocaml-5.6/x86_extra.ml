(* The instructions of [X86_ast] that only exist in OCaml >= 5.6 (see
   [X86_encoder.encode_extra]). The type [t] is the same in
   [ocaml-5.3/x86_extra.ml], where no instruction is recognized. *)

type t =
  | Bsf of X86_ast.arg * X86_ast.arg
  | Bsr of X86_ast.arg * X86_ast.arg
  | Bts of X86_ast.arg * X86_ast.arg
  | Xadd of X86_ast.arg * X86_ast.arg
  | Lock_xadd of X86_ast.arg * X86_ast.arg

let classify (i : X86_ast.instruction) : t option =
  match i with
  | BSF (src, dst) -> Some (Bsf (src, dst))
  | BSR (src, dst) -> Some (Bsr (src, dst))
  | BTS (src, dst) -> Some (Bts (src, dst))
  | XADD (src, dst) -> Some (Xadd (src, dst))
  | LOCK_XADD (src, dst) -> Some (Lock_xadd (src, dst))
  | _ -> None
