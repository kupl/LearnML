(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam -> 
  let rec checker lamb set = let rec probe x set = 
                      match set with
                      | [] -> false
                      | hd::tl -> if x = hd then true else probe x tl in 
                         match lamb with
                          | V v -> probe v set
                          | P (v, lamb) -> let extend x set = x :: set in 
                                           let set2 = extend v set in checker lamb set2
                          | C (l1, l2) -> checker l1 set && checker l2 set 
                            in checker lam []
(*
let t1 = P ("a", V "a")
let t2 = P ("a", P ("a", V "a"))
let t3 = P ("a", P ("b", C (V "a", V "b")))
let t4 = P ("a", C (V "a", P ("b", V "a")))

let f1 = P ("a", V "b")
let f2 = P ("a", C (V "a", P ("b", V "c")))
let f3 = P ("a", P ("b", C (V "a", V "c")))

let test = LET("y", CONST 3, 
  LETREC("f", "x", 
    IF (ISZERO (VAR "x"), CONST 1, MUL( CALL (VAR "f", SUB (VAR "x", CONST 1)), VAR "x")), 
      MUL(CALL(VAR "f", CONST 3), VAR "y")))
*)