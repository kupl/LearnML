type metro = STATION of name
  | AREA of name * metro
  | CONNECT of metro * metro
and name = string

(* let rec checkConnect1(input1,inputlist) = match input1 with
  |STATION a -> if List.mem a inputlist then true else false
  |AREA(a,b) -> checkArea(b, (addtolist inputlist a))
  |CONNECT(a,b) *)

(* let rec checkConnect2(input2,inputlist) = match input2 with  *)

let addtolist inputlist element = List.append [element] inputlist

let rec checkArea (input, inputarray) = match input with
  |STATION b -> if List.mem b inputarray then true else false
  |AREA(a,b)-> checkArea(b,(addtolist inputarray a))
  |CONNECT(a,b) -> if checkArea(a, inputarray) && checkArea(b, inputarray) then true else false

let rec checkMetroHelp input inputarray = match input with
  |AREA(a,b) -> checkArea(b,(addtolist inputarray a))
  |STATION x -> false
  |CONNECT(a,b) -> if checkArea(a, inputarray) && checkArea(b, inputarray) then true else false

let checkMetro input = checkMetroHelp input []


(* let _ =
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
  test_case(10, false == checkMetro(STATION "a")) *)
