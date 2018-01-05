type metro = STATION of name
            | AREA of name * metro
            | CONNECT of metro * metro
and name = string

let checkMetro: metro -> bool = fun(m) ->
  let rec isNameInList: name * name list -> bool = fun (n, nl) ->
    match nl with
      |[] -> false
      |nh :: nt -> (nh = n) || isNameInList(n, nt)
  in
  let rec checkMetroWithNameSpace: metro * name list -> bool = fun (m, nl) ->
    match m with
      |STATION id -> isNameInList(id, nl)
      |CONNECT(a, b) -> checkMetroWithNameSpace(a, nl) && checkMetroWithNameSpace(b, nl)
      |AREA(id, mt) -> checkMetroWithNameSpace(mt, id :: nl)
  in
  checkMetroWithNameSpace(m, [])

(* TEST *)
(*
let _ = 
  let test_case : int * bool -> unit = fun (n, x) -> 
    print_endline ("Case " ^ string_of_int(n) ^ " : " ^ string_of_bool(x)) in 
  test_case(1, true == checkMetro(AREA("a", STATION "a"))); 
  test_case(2, true == checkMetro(AREA("a", AREA("a", STATION "a")))); 
  test_case(3, true == checkMetro(AREA("a", AREA("b", CONNECT(STATION "a", STATION "b"))))); 
  test_case(4, true == checkMetro(AREA("a", CONNECT(STATION "a", AREA("b", STATION "a"))))); 
  test_case(5, false == checkMetro(AREA("a", STATION "b"))); 
  test_case(6, false == checkMetro(AREA("a", CONNECT(STATION "a", AREA("b", STATION "c"))))); 
  test_case(7, false == checkMetro(AREA("a", AREA("b", CONNECT(STATION "a", STATION "c"))))); 
  test_case(8, true == checkMetro(CONNECT(AREA("a", STATION "a"), AREA("b", AREA("a", CONNECT(STATION "b", STATION "a")))))); 
  test_case(9, false == checkMetro(CONNECT(AREA("c", STATION "c"), AREA("b", AREA("a", CONNECT(STATION "b", STATION "c")))))); 
  test_case(10, false == checkMetro(STATION "a"))
*)
