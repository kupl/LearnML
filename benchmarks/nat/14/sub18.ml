type nat = 
    | ZERO    (* 0 *)
    | SUCC of nat (* 0/ *)

let rec natadd (n1, n2) = 
    match n1, n2 with
    | ZERO, ZERO -> ZERO
    | ZERO, SUCC a -> SUCC a
    | SUCC a, ZERO -> SUCC a
    | SUCC a, SUCC b -> SUCC(SUCC (natadd (a, b)))


let rec natmul (n3, n4) = 
    match n3, n4 with
    | ZERO, ZERO -> ZERO
    | ZERO, SUCC a -> ZERO
    | SUCC a, ZERO -> ZERO
    | SUCC ZERO, SUCC ZERO -> SUCC ZERO
    | SUCC ZERO, SUCC a -> SUCC a
    | SUCC a, SUCC ZERO -> SUCC a
    | SUCC a, SUCC b -> (natadd (SUCC a, (natmul (SUCC a, b))))


(*
natadd (SUCC(SUCC(SUCC ZERO)), SUCC(SUCC ZERO));;
natadd (ZERO, SUCC(ZERO));;
natadd (SUCC(ZERO), ZERO);;

natmul (SUCC(SUCC(SUCC ZERO)), SUCC(SUCC ZERO));; (* 3*2 *)
natmul (ZERO, SUCC(ZERO));; (* 0*1 *)
natmul (SUCC(ZERO), ZERO);; (* 1*0 *)

natmul (SUCC(SUCC(SUCC ZERO)), SUCC ZERO);; (* 3*1 *)
natmul (SUCC ZERO, SUCC(SUCC ZERO));; (* 1*2 *)
natmul (SUCC(SUCC(SUCC ZERO)), SUCC(SUCC(SUCC(SUCC(SUCC ZERO))))) (* 3*5 *)
*)
