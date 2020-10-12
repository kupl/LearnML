type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam -> 
  let rec lookup_env : var list -> var -> bool
  = fun env v ->
    (match env with 
      | [] -> false
      | hd::tl -> (hd = v) || (lookup_env tl v)
      )
    in
    let rec check' : lambda -> var list -> bool
    = fun lam env ->
      (match lam with
        | V v -> lookup_env env v
        | P (v,l) -> check' l (v::env)
        | C (l1,l2) -> (check' l1 env) && (check' l2 env)
      )
  in check' lam []



(*******************************************************************************************************)
let l1 = P ("a", V "a");;
let l2 = P ("a", P ("a", V "a"));;
let l3 = P ("a", P ("b", C (V "a", V "b")));;
let l4 = P ("a", C (V "a", P ("b", V "a")));;
let i1 = P ("a", V "b");;
let i2 = P ("a", C (V "a", P ("b", V "c")));;
let i3 = P ("a", P ("b", C (V "a", V "c")));;
let test1 = P("x", V"x");;
let test2 = C(P("x", V "x"), V "y");;
let test3 = P("x", C(V "x", V"y"));;
let tets4 = C(P("x", C(V "x", V"y")), V "z");;
let test5 = C(P ("x", P("y", V "x")), V "z");;
  
check test1;;