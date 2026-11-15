(* Values of the program used by the evaluated code. *)

let results : string list ref = ref []

let record (s : string) : unit = results := s :: !results
