type lambda = V of var | P of var * lambda | C of lambda * lambda
and var = string

let rec remainStation : lambda -> string list = fun m ->
        match m with
        |V(var) -> [var]
        |P(var,lambda) ->
                let f : string -> bool = fun i -> (i<>var) in
                let rm = remainStation(lambda) in
                List.filter f rm
        |C(lambda1,lambda2) -> remainStation(lambda1)@remainStation(lambda2)

let check : lambda -> bool = fun m ->
        match remainStation(m) with
        |[] -> true
        |head::tail -> false
(*
let a81 = check (P("a", V "a")) 
let a82 = check (P("a", P("a", V "a"))) 
let a83 = check (P("a", P("b", C(V "a", V "b")))) 
let a84 = check (P("a", C(V "a", P("b", V "a")))) 

let a85 = check (P("a", V "b")) 
let a86 = check (P("a", C(V "a", P("b", V "c")))) 
let a87 = check (P("a", P("b", C(V "a", V "c")))) 
let a88 = check (P ("a", C (P ("b", V "a"), P("c", V "b"))))

let _ = print_endline(string_of_bool a81)
let _ = print_endline(string_of_bool a82)
let _ = print_endline(string_of_bool a83)
let _ = print_endline(string_of_bool a84)
let _ = print_endline(string_of_bool a85)
let _ = print_endline(string_of_bool a86)
let _ = print_endline(string_of_bool a87)
let _ = print_endline(string_of_bool a88)


let t1 = P("a", C(V "a", P("b", V "a")))
let t2 = P("a", P("b", C(V "a", V "b")))
let t3 = C( P ("a", V "a"), P ("b", V "b"))
let t4 = P("a", P("b", P("c", P("d", P("e", V "a")))))
let t5 = P("a", P("b", P("c", P("d", P("e", V "c")))))
let t6 = P("a", P("a", V "a"))
let t7 = C( C( P( "a", V "a"), P("b", V "b")),C( P( "a", V "a"), P("b", V "b")))
let t8 = P("a", V "a")

let tc1 = check t1
let tc2 = check t2
let tc3 = check t3
let tc4 = check t4
let tc5 = check t5
let tc6 = check t6
let tc7 = check t7
let tc8 = check t8
let _ = assert(tc1=true)
let _ = assert(tc2=true)
let _ = assert(tc3=true)
let _ = assert(tc4=true)
let _ = assert(tc5=true)
let _ = assert(tc6=true)
let _ = assert(tc7=true)
let _ = assert(tc8=true)

let f1 = P("a", C(V "a", P("b", V "c")))
let f2 = P("a", P("c", C(V "a", V "b")))
let f3 = C( P ("a", V "a"), P ("b", V "a"))
let f4 = P("a", P("b", P("c", P("d", P("e", V "f")))))
let f5 = P("a", P("b", P("c", P("d", P("e", V "q")))))
let f6 = P("a", P("a", V "b"))
let f7 = C( C( P( "a", V "a"), P("b", V "f")),C( P( "a", V "a"), P("b", V "b")))
let f8 = V "a"

let _ = assert((check f1)=false)
let _ = assert((check f2)=false)
let _ = assert((check f3)=false)
let _ = assert((check f4)=false)
let _ = assert((check f5)=false)
let _ = assert((check f6)=false)
let _ = assert((check f7)=false)
let _ = assert((check f8)=false)*)
