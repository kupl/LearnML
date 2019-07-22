(* 4190,310 Programming Language (Fall 2014)
 * Homework 2 - Exercise 1
 * CSE / 2012-13456 / Gao, Chengbin *)

type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string

let checkMetro met =
    let rec aux met under = 
        match met with
        | CONNECT(m1, m2) ->
                (aux m1 under) && (aux m2 under)
        | AREA(n, m) ->
                aux m (n :: under)
        | STATION(n) ->
                List.mem n under
    in aux met []
              
(*
let t1 = AREA("a", STATION "a")
let t2 = AREA("a", AREA("a", STATION "a"))
let t3 = AREA("a", AREA("b", CONNECT(STATION "a", STATION "b")))
let t4 = AREA("a", CONNECT(STATION "a", AREA("b", STATION "a")))
let f1 = AREA("a", STATION "b")
let f2 = AREA("a", CONNECT(STATION "a", AREA("b", STATION "c")))
let f3 = AREA("a", AREA("b", CONNECT(STATION "a", STATION "c")))
let print_bool x = print_endline(string_of_bool x)
let _ =
    print_bool (checkMetro t1) ;
    print_bool (checkMetro t2) ;
    print_bool (checkMetro t3) ;
    print_bool (checkMetro t4) ;
    print_bool (not (checkMetro f1)) ;
    print_bool (not (checkMetro f2)) ;
    print_bool (not (checkMetro f3)) ;
*)
