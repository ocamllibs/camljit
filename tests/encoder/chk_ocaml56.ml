(* Functions that OCaml 5.6 compiles to instructions that the modules of the
   standard library checked here do not use. On arm64: [clz], [rbit] (count
   of the leading and trailing zeros) and [frinta], [frintp], [frintm],
   [frintz] (rounding). On amd64: [bsr], [bsf] and [bts] (count of the
   leading and trailing zeros), and [xadd] and [lock xadd]
   ([Atomic.fetch_and_add]). With OCaml 5.5, they are calls to C functions. *)

let clz x = Int.leading_zeros x

let ctz x = Int.trailing_zeros x

let round x = Float.round x

let ceil x = Float.ceil x

let floor x = Float.floor x

let trunc x = Float.trunc x

let fetch_and_add r n = Atomic.fetch_and_add r n
