(* Dept. of Computer Science and Engineering, 2015-12055, An Dantae, 2-4 *)
type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
and var = string

let rec helper: lambda * var list -> bool = fun (a, l) -> 
    match a with
    | V p -> List.mem p l
    | C (s, t) -> helper(s, l) && helper(t, l)
    | P (p, s) -> helper(s, p::l)

let check: lambda -> bool = fun a -> helper(a, [])

(* Test Code 
let x : lambda = P("a", V "a")
let y : lambda = P("a", P("b", C(V "a", V "b")))
let z : lambda = P("a", C(V "a", P("b", V "a")))
let w : lambda = P("a", C(V "a", P("b", V "b")))
let t : lambda = P("a", C(P("b", V "a"), P("c", V "c")))
let u : lambda = P("c", P("a", C(V "a", V "a")))
let x' : lambda = V "a"
let y' : lambda = C(P("a", V "a"), V("a"))
let z' : lambda = P("a", C(V "a", P("b", V "c")))
let w' : lambda = P("b", C(P("a", V "a"), V "a"))
let t' : lambda = P("a", P("b", C(V "a", V "c")))
let u' : lambda = P("a", C(V "b", P("b", V "a")))

let test m = match check(m) with
    | true -> print_endline("true")
    | false -> print_endline("false")

let _ = test(x)
let _ = test(y)
let _ = test(z)
let _ = test(w)
let _ = test(t)
let _ = test(u)
let _ = test(P("c",C(x, w)))
let _ = test(P("c", t'))
let _ = print_newline ()
let _ = test(x')
let _ = test(y')
let _ = test(z')
let _ = test(w')
let _ = test(t')
let _ = test(u')
*)
