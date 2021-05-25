type lambda = V of var
| P of var * lambda
| C of lambda * lambda
and var = string

let check m = 
  let rec check2 m l = 
    match m with
    | V a -> List.mem a l
    | P(var, lambda) -> (check2 lambda (var::l)) 
    | C(m1,m2) -> (check2 m1 l) && (check2 m2 l)
  in
  check2 m []

  
  (*
let a = P("a", V "a")
let b = P("a", P("a", V "a"))
let c = P("a", P("b", C(V "a", V "b")))
let d = P("a", C(V "a", P("b", V "a")))
let e = P("a", V "b")
let f = P("a", C(V "a", P("b", V "c")))
let g = P("a", P("b", C(V "a", V "c")))

let l = [a;b;c;d;e;f;g]

let print_bool a =
  match a with
  | true -> print_endline "TRUE"
  | false -> print_endline "FALSE"

let _ = List.iter print_bool (List.map check l)

*)
