(* 4190,310 Programming Language (Fall 2014)
 * Homework 2 - Exercise 1
 * CSE / 2012-13456 / Gao, Chengbin *)

type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
and var = string

let check met =
    let rec aux met under = 
        match met with
        | C(m1, m2) ->
                (aux m1 under) && (aux m2 under)
        | P(n, m) ->
                aux m (n :: under)
        | V(n) ->
                List.mem n under
    in aux met []
              
(*
let t1 = P("a", V "a")
let t2 = P("a", P("a", V "a"))
let t3 = P("a", P("b", C(V "a", V "b")))
let t4 = P("a", C(V "a", P("b", V "a")))
let f1 = P("a", V "b")
let f2 = P("a", C(V "a", P("b", V "c")))
let f3 = P("a", P("b", C(V "a", V "c")))
let print_bool x = print_endline(string_of_bool x)
let _ =
    print_bool (check t1) ;
    print_bool (check t2) ;
    print_bool (check t3) ;
    print_bool (check t4) ;
    print_bool (not (check f1)) ;
    print_bool (not (check f2)) ;
    print_bool (not (check f3)) ;
*)
