(* 컴퓨터공학부 2013-11425 이창영 hw1_4 *)

type nat = ZERO
          |SUCC of nat

let rec natadd ((a : nat), (b : nat)) : nat =
    match (a,b) with
    |(ZERO, ZERO) -> ZERO
    |(ZERO, SUCC (d)) -> b
    |(SUCC (c), ZERO) -> a
    |(SUCC (c), SUCC (d)) -> SUCC (SUCC (natadd (c, d)))

let rec natmul2 ((a : nat), (b : nat), (res : nat)) : nat =
    match (a,b) with
    |(ZERO, ZERO) -> ZERO
    |(ZERO, SUCC (d)) -> ZERO
    |(SUCC (c), ZERO) -> res
    |(SUCC (c), SUCC (d)) -> natmul2 (a, d, (natadd (res, a)))

let natmul ((a : nat), (b : nat)) : nat =
    natmul2 (a, b, ZERO)
