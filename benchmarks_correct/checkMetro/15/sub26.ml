type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var =
  string
;;

let check lambda =
  let rec checkWithNamespace (sub_lambda, varspace) =
    match sub_lambda with
    | V n ->
        List.mem n varspace
    | P (n, m) ->
        checkWithNamespace (m, n :: varspace)
    | C(m1, m2) ->
        checkWithNamespace (m1, varspace) && checkWithNamespace (m2, varspace)
  in checkWithNamespace (lambda, [])
;;

(* Test Case *)
(*
let _ =
  let print_bool x =
    print_endline (string_of_bool x)
  in

  print_bool (true = check ( C (P ("a", V "a"), P ("b", P("a", C(V "b", V "a"))))));
  print_bool (false = check ( C (P ("c", V "c"), P ("b", P("a", C(V "b", V "c"))))));
  print_bool (true = check ( P ("a", V "a")));
  print_bool (true = check ( P("a", P("a", V "a"))));
  print_bool (true = check ( P("a", P("b", C(V "a", V "b")))));
  print_bool (true = check ( P("a", C(V "a", P("b", V "a")))));
  print_bool (false = check ( P("a", V "b")));
  print_bool (false = check ( P("a", C(V "a", P("b", V "c")))));
  print_bool (false = check ( P("a", P("b", C(V "a", V "c")))));
  *)
