
type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
        and var = string

let check: lambda -> bool = fun(m) ->
    let rec checkInternal: string list * lambda -> bool = fun(sl, m) ->
        match m with
        | V(n) -> List.mem n sl
        | P(n, m) -> checkInternal(n::sl, m)
        | C(m1, m2) -> checkInternal(sl, m1) && checkInternal(sl, m2)
        in
    checkInternal([], m)


(*

let t1 = P("a", V "a")
let t2 = P("a", P("a", V "a"))
let t3 = P("a", P("b", C(V "a", V "b")))
let t4 = P("a", C(V "a", P("b", V "a")))

let f1 = P("a", V "b")
let f2 = P("a", C(V "a", P("b", V "c")))
let f3 = P("a", P("b", C(V "a", V "c")))

let _ = print_endline(string_of_bool(check(t1)))
let _ = print_endline(string_of_bool(check(t2)))
let _ = print_endline(string_of_bool(check(t3)))
let _ = print_endline(string_of_bool(check(t4)))
let _ = print_endline(string_of_bool(check(f1)))
let _ = print_endline(string_of_bool(check(f2)))
let _ = print_endline(string_of_bool(check(f3)))
*)
