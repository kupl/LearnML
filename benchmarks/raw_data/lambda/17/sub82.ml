type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let check: lambda -> bool = fun(m) ->
  let rec isNameInList: var * var list -> bool = fun (n, nl) ->
    match nl with
      |[] -> false
      |nh :: nt -> (nh = n) || isNameInList(n, nt)
  in
  let rec checkWithNameSpace: lambda * var list -> bool = fun (m, nl) ->
    match m with
      |V id -> isNameInList(id, nl)
      |C(a, b) -> checkWithNameSpace(a, nl) && checkWithNameSpace(b, nl)
      |P(id, mt) -> checkWithNameSpace(mt, id :: nl)
  in
  checkWithNameSpace(m, [])

(* TEST *)
(*
let _ = 
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
  test_case(10, false == check(V "a"))
*)
