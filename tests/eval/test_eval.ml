let parse (source : string) : Parsetree.structure =
  Parse.implementation (Lexing.from_string source)

let initial_env () : Env.t =
  Clflags.native_code := true;
  (* The directory of the interface of [Test_eval_api] is given on the
     command line. *)
  Clflags.include_dirs := [Sys.argv.(1)];
  Compmisc.init_path ();
  Compmisc.initial_env ()

let check (name : string) (expected : string list) : unit =
  let results = List.rev !Test_eval_api.results in
  if results <> expected then
    begin
      Printf.eprintf "%s: got [%s], expected [%s]\n" name
        (String.concat "; " results) (String.concat "; " expected);
      exit 1
    end;
  Test_eval_api.results := []

let main () =
  assert Camljit.Compile.available;
  let env = initial_env () in
  let eval source = Camljit.Compile.eval env (parse source) in
  eval {| let () = Test_eval_api.record "hello" |};
  check "simple" ["hello"];
  (* Partial applications and closures: generic functions caml_curryN and
     caml_applyN. *)
  eval {|
    let f a b c d e = String.concat "," [a; b; c; d; e]
    let g = f "1" "2"
    let h = g "3"
    let () = Test_eval_api.record (h "4" "5")
    let () =
      List.iter Test_eval_api.record
        (List.map (fun (x, y) -> x ^ y) [("a", "b"); ("c", "d")]) |};
  check "closures" ["1,2,3,4,5"; "ab"; "cd"];
  (* Allocation and the GC, with the frame table of the unit registered. *)
  eval {|
    let l = List.init 100_000 (fun i -> string_of_int i)
    let () = Gc.full_major ()
    let () = Test_eval_api.record (List.nth l 99_999)
    let () = Gc.compact () |};
  check "gc" ["99999"];
  (* Exceptions raised by the initialization of a unit. *)
  begin match eval {| let () = raise (Failure "from unit") |} with
  | () -> prerr_endline "exception: not raised"; exit 1
  | exception Failure msg -> Test_eval_api.record msg
  end;
  check "exception" ["from unit"];
  (* Typing errors are raised as by the compiler. *)
  begin match eval {| let () = Test_eval_api.record 1 |} with
  | () -> prerr_endline "typing error: not raised"; exit 1
  | exception exn ->
     match Location.error_of_exn exn with
     | Some (`Ok _) -> Test_eval_api.record "typing error"
     | _ -> raise exn
  end;
  check "typing error" ["typing error"];
  (* Named units, and units evaluated after the others. *)
  Camljit.Compile.eval ~module_name:"Named_unit" env
    (parse {| let () = Test_eval_api.record "named" |});
  check "named" ["named"];
  for i = 1 to 50 do
    eval (Printf.sprintf {| let () = Test_eval_api.record "%d" |} i)
  done;
  check "many" (List.init 50 (fun i -> string_of_int (i + 1)))

let () =
  try main ()
  with exn ->
    Location.report_exception Format.err_formatter exn;
    exit 1
