(* 4190.310 Programming Languages - Daegeun Lee <elnn@elnn.kr> *)

type nat = ZERO | SUCC of nat

let rec natadd (a, b) =
    match a with
    | ZERO -> b
    | SUCC x -> SUCC (natadd(x, b))

let rec natmul (a, b) =
    match a with
    | ZERO -> ZERO
    | SUCC x -> natadd(b, natmul(x, b))

