(* Dept. of Computer Science and Engineering, 2015-12055, An Dantae, 2-4 *)
type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string

let rec helper: metro * name list -> bool = fun (a, l) -> 
    match a with
    | STATION p -> List.mem p l
    | CONNECT (s, t) -> helper(s, l) && helper(t, l)
    | AREA (p, s) -> helper(s, p::l)

let checkMetro: metro -> bool = fun a -> helper(a, [])

(* Test Code 
let x : metro = AREA("a", STATION "a")
let y : metro = AREA("a", AREA("b", CONNECT(STATION "a", STATION "b")))
let z : metro = AREA("a", CONNECT(STATION "a", AREA("b", STATION "a")))
let w : metro = AREA("a", CONNECT(STATION "a", AREA("b", STATION "b")))
let t : metro = AREA("a", CONNECT(AREA("b", STATION "a"), AREA("c", STATION "c")))
let u : metro = AREA("c", AREA("a", CONNECT(STATION "a", STATION "a")))
let x' : metro = STATION "a"
let y' : metro = CONNECT(AREA("a", STATION "a"), STATION("a"))
let z' : metro = AREA("a", CONNECT(STATION "a", AREA("b", STATION "c")))
let w' : metro = AREA("b", CONNECT(AREA("a", STATION "a"), STATION "a"))
let t' : metro = AREA("a", AREA("b", CONNECT(STATION "a", STATION "c")))
let u' : metro = AREA("a", CONNECT(STATION "b", AREA("b", STATION "a")))

let test m = match checkMetro(m) with
    | true -> print_endline("true")
    | false -> print_endline("false")

let _ = test(x)
let _ = test(y)
let _ = test(z)
let _ = test(w)
let _ = test(t)
let _ = test(u)
let _ = test(AREA("c",CONNECT(x, w)))
let _ = test(AREA("c", t'))
let _ = print_newline ()
let _ = test(x')
let _ = test(y')
let _ = test(z')
let _ = test(w')
let _ = test(t')
let _ = test(u')
*)
