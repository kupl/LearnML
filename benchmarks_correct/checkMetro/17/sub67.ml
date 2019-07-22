type metro = STATION of name
				|AREA of name * metro
				|CONNECT of metro * metro
  and name = string

let make_list( (n : name), (l : name list) ) : name list = 
		n :: l

let rec checkM ( (m:metro),(l:name list) ) : bool = 
		match m with
		|AREA (n,ml) -> checkM(ml, make_list (n,l))
		|STATION n -> List.mem n l
		|CONNECT (m1,m2) -> checkM(m1, l) && checkM(m2, l)

let checkMetro (m:metro) : bool = 
		checkM (m,[])

(*-------------------test case----------------*)
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
