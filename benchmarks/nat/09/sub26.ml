(* School of Computer Science & Engineering
 * 2009-23151
 * 조성근
 * HW 1 - Exercise 5
 *)

type nat = ZERO | SUCC of nat;;

let rec natadd (a,b) = match a with
    ZERO -> b
  | SUCC (n) -> SUCC(natadd (n,b));;

let rec natmul (a,b) = match a with
    ZERO -> ZERO
  | SUCC (n) -> natadd(b, natmul(n,b));;
