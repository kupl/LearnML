type lambda = V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

(* let rec checkConnect1(input1,inputlist) = match input1 with
  |V a -> if List.mem a inputlist then true else false
  |P(a,b) -> checkArea(b, (addtolist inputlist a))
  |C(a,b) *)

(* let rec checkConnect2(input2,inputlist) = match input2 with  *)

let addtolist inputlist element = List.append [element] inputlist

let rec checkArea (input, inputarray) = match input with
  |V b -> if List.mem b inputarray then true else false
  |P(a,b)-> checkArea(b,(addtolist inputarray a))
  |C(a,b) -> if checkArea(a, inputarray) && checkArea(b, inputarray) then true else false

let rec checkHelp input inputarray = match input with
  |P(a,b) -> checkArea(b,(addtolist inputarray a))
  |V x -> false
  |C(a,b) -> if checkArea(a, inputarray) && checkArea(b, inputarray) then true else false

let check input = checkHelp input []


(* let _ =
  let test_case : int * bool -> unit = fun (n, x) ->
    print_endline ("Case " ^ string_of_int(n) ^ " : " ^ string_of_bool(x)) in
  test_case(1, true == check(P("a", V "a")));
  test_case(2, true == check(P("a", P("a", V "a"))));
  test_case(3, true == check(P("a", P("b", C(V "a", V "b")))));
  test_case(4, true == check(P("a", C(V "a", P("b", V "a")))));
  test_case(5, false == check(P("a", V "b")));
  test_case(6, false == check(P("a", C(V "a", P("b", V "c")))));
  test_case(7, false == check(P("a", P("b", C(V "a", V "c")))));
  test_case(8, true == check(C(P("a", V "a"), P("b", P("a", C(V "b", V "a"))))));
  test_case(9, false == check(C(P("c", V "c"), P("b", P("a", C(V "b", V "c"))))));
  test_case(10, false == check(V "a")) *)
