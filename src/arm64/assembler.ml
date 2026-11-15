(* Assembly in memory, on arm64 (see [Compile]): the arm64 backend of the
   compiler ([Emit]) prints the assembly code, which is then assembled by an
   external command. To avoid it, the compilation stops before the emission
   of the code ([-stop-after scheduling]), saving the linear code of the unit
   ([-save-ir-after scheduling]); the code is then emitted from the linear
   code, as by [ocamlopt -start-from emit], and the assembly code is encoded
   by [Arm64_encoder]. *)

module Compiler_pass = Clflags.Compiler_pass

let available = true

let with_value (r : 'a ref) (v : 'a) (f : unit -> 'b) : 'b =
  let saved = !r in
  r := v;
  Fun.protect f ~finally:(fun () -> r := saved)

(* [-save-ir-after scheduling] during [f]. *)
let with_linear_saved (f : unit -> 'a) : 'a =
  let saved = Clflags.should_save_ir_after Compiler_pass.Scheduling in
  Clflags.set_save_ir_after Compiler_pass.Scheduling true;
  Fun.protect f ~finally:(fun () ->
    Clflags.set_save_ir_after Compiler_pass.Scheduling saved)

let remove (filename : string) : unit =
  try Sys.remove filename with Sys_error _ -> ()

let assemble_in_memory ~(output_prefix : string) (f : unit -> unit)
    : Object.t =
  let linear = Compiler_pass.(to_output_filename Scheduling ~prefix:output_prefix) in
  let asm = output_prefix ^ ".jit.s" in
  Fun.protect ~finally:(fun () -> remove linear; remove asm) (fun () ->
    (* The code that [f] would emit directly (the beginning and the end of
       the units of generic functions) is discarded: it is emitted again
       below. *)
    Out_channel.with_open_bin Filename.null (fun null ->
      with_value Emitaux.output_channel null (fun () ->
        with_value Clflags.stop_after (Some Compiler_pass.Scheduling) (fun () ->
          with_linear_saved f)));
    let unit_info, _ = Linear_format.restore linear in
    Out_channel.with_open_bin asm (fun oc ->
      with_value Emitaux.output_channel oc (fun () ->
        Emit.begin_assembly ();
        List.iter (function
          | Linear_format.Data dl -> Emit.data dl
          | Func f -> Emit.fundecl f) unit_info.items;
        Emit.end_assembly ()));
    Arm64_encoder.assemble (In_channel.with_open_bin asm In_channel.input_all))
