(* PL HW5-6 "자연수" 
   2007-11738
   알렉산더 *)

type nat = ZERO | SUCC of nat

let rec natadd (n1, n2) = 
    match (n1, n2) with
        (n1, ZERO) -> n1
       |(ZERO, n2) -> n2
       |(n11, SUCC n22) -> (natadd (SUCC n11, n22))

let natmul (n1, n2) =
    let rec natmulInner (nat1, nat2) =
        match (nat1, nat2) with
            (n11, ZERO) -> ZERO
           |(ZERO, n22) -> ZERO
           |(n11, SUCC ZERO) -> n11
           |(SUCC ZERO, n22) -> n22
           |(n11, SUCC n22) -> (natmulInner (natadd (n11, n1), n22))
    in
    natmulInner (n1, n2)


(* ================================================================
(** int to nat transformation **)
let rec int2nat i =
    if i <= 0 then ZERO
    else SUCC (int2nat (i-1))

(** nat to int transformation **)
let rec nat2int n =
    match n with
        ZERO -> 0
       |SUCC n1 -> 1 + (nat2int n1)
*)
