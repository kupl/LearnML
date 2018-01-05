type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
and name = string

let rec checkArea a = 
	match a with
	([], STATION n) -> false
	|(l, STATION n) -> List.mem n l
	|(l, AREA(n, m)) -> checkArea (n::l, m)
	|(l, CONNECT(m1, m2)) -> checkArea(l, m1) && checkArea(l, m2) 

let rec checkMetro : metro -> bool = fun ch ->
	match ch with  
	 STATION n -> false
	 | CONNECT (m1, m2) -> (checkMetro m1) && (checkMetro m2)
	 | AREA (n, m) -> 
	 	checkArea([n], m)
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
