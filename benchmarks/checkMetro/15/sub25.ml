type metro =
  | STATION of name
  | AREA of name * metro
  | CONNECT of metro * metro
and name =
  string
;;

let checkMetro metro =
  let rec checkMetroWithNamespace (sub_metro, namespace) =
    match sub_metro with
    | STATION n ->
        List.mem n namespace
    | AREA (n, m) ->
        checkMetroWithNamespace (m, n :: namespace)
    | CONNECT(m1, m2) ->
        checkMetroWithNamespace (m1, namespace) && checkMetroWithNamespace (m2, namespace)
  in checkMetroWithNamespace (metro, [])
;;

(* Test Case *)
(*
let _ =
  let print_bool x =
    print_endline (string_of_bool x)
  in

  print_bool (true = checkMetro ( CONNECT (AREA ("a", STATION "a"), AREA ("b", AREA("a", CONNECT(STATION "b", STATION "a"))))));
  print_bool (false = checkMetro ( CONNECT (AREA ("c", STATION "c"), AREA ("b", AREA("a", CONNECT(STATION "b", STATION "c"))))));
  print_bool (true = checkMetro ( AREA ("a", STATION "a")));
  print_bool (true = checkMetro ( AREA("a", AREA("a", STATION "a"))));
  print_bool (true = checkMetro ( AREA("a", AREA("b", CONNECT(STATION "a", STATION "b")))));
  print_bool (true = checkMetro ( AREA("a", CONNECT(STATION "a", AREA("b", STATION "a")))));
  print_bool (false = checkMetro ( AREA("a", STATION "b")));
  print_bool (false = checkMetro ( AREA("a", CONNECT(STATION "a", AREA("b", STATION "c")))));
  print_bool (false = checkMetro ( AREA("a", AREA("b", CONNECT(STATION "a", STATION "c")))));
  *)
