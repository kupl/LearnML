(*
 * 2017 - 09 - 22
 * PL Homework 2-4
 * Joonmo Yang
*)

type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
and var = string

let check : lambda -> bool =
  fun mt ->
  ( 
    let rec check_sub : lambda * var list -> bool =
      fun (m, nlist) ->
      ( match m with
        | V s -> (List.exists (fun x -> x = s) nlist)
        | P (nm, mtr) -> 
          if (List.exists (fun x -> x = nm) nlist) then check_sub(mtr, nlist)
          else check_sub(mtr, nm::nlist)
        | C (mtr1, mtr2) -> check_sub(mtr1, nlist) && check_sub(mtr2, nlist)
      )
    in check_sub(mt, [])
  )

(* test cases
let a1 = P("a", V "a")
let a2 = P("a", P("a", V "a"))
let a3 = P("a", P("b", C(V "a", V "b")))
let a4 = P("a", C(V "a", P("b", V "a")))

let b1 = check a1
let b2 = check a2
let b3 = check a3
let b4 = check a4

let a5 = P("a", V "b")
let a6 = P("a", C(V "a", P("b", V "c")))
let a7 = P("a", P("b", C(V "a", V "c")))

let b5 = check a5
let b6 = check a6
let b7 = check a7

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
