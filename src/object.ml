(* Machine code of a compilation unit, as produced by the internal assembler
   ([X86_encoder] on amd64, [Arm64_encoder] on arm64) and loaded in memory by
   [Loader]: the equivalent of an object file, with sections, symbols and
   relocations. *)

(* The version of the format and of the encoding, part of the keys of the
   units in the cache (where the objects are stored marshalled): it has to
   be incremented when [t] or the code produced by the encoder changes. *)
let version = 2

type relocation_kind =
  | Absolute64
    (** The 64-bit address of the target. *)
  | Absolute32_signed
    (** The 32-bit address of the target, sign-extended (only valid if the
        target is in the first 2 GB of memory). *)
  | Relative32
    (** The 32-bit displacement to the target from the address of the
        field. *)
  | Branch32
    (** As [Relative32], for the target of a call or a jump, which can go
        through a stub if the target is too far. *)
  | Got_relative32
    (** The 32-bit displacement to an entry of the global offset table that
        contains the address of the target. *)
  | Arm64_branch26
    (** The displacement to the target, in 32-bit words, in the 26-bit field
        of a [b] or [bl] instruction (which can go through a stub if the
        target is too far). *)
  | Arm64_page21
    (** The displacement between the 4 KB pages of the target and of the
        instruction, in the 21-bit field of an [adrp] instruction. *)
  | Arm64_pageoff12 of int
    (** The offset of the target in its 4 KB page, shifted right by the given
        amount (the log2 of the size of the access), in the 12-bit field of an
        [add] or of a load or store instruction. *)
  | Arm64_got_page21
    (** As [Arm64_page21], for an entry of the global offset table that
        contains the address of the target. *)
  | Arm64_got_pageoff12
    (** As [Arm64_pageoff12 3], for an entry of the global offset table that
        contains the address of the target. *)

type relocation = {
    offset : int;
      (** Offset of the field in the section. *)
    kind : relocation_kind;
    target : string;
      (** Name of the target symbol. *)
    addend : int64;
      (** The value of the field is [target + addend], minus the address of
          the field (or of its page) for the relative kinds. *)
  }

type section = {
    name : string;
    contents : string;
    alignment : int;
    executable : bool;
    relocations : relocation list;
  }

type symbol = {
    section : int;
      (** Index of the section that defines the symbol. *)
    position : int;
      (** Offset of the symbol in the section. *)
    global : bool;
  }

type t = {
    sections : section array;
    symbols : (string * symbol) list;
  }
