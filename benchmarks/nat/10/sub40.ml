(* 4190.310 Programming Language			*
 * Homework #1 - Exercise 6 (자연수)		*
 * 2008-11744 Jongwook Choi 				*)

type nat = ZERO | SUCC of nat

let rec natadd (lhs, rhs) = 
match rhs with
	  ZERO -> lhs
	| SUCC t -> natadd (SUCC lhs, t)

let rec natmul (lhs, rhs) =
match rhs with
	  ZERO -> ZERO
	| SUCC t -> natadd ( natmul(lhs, t), lhs )


(* TEST CODE *)
(*

let zero = ZERO ;;
let one = SUCC zero;; 
let two = SUCC one;;
let three = SUCC two;;
let four = SUCC three;;
let five = SUCC four ;;
let six = SUCC five ;;
let seven = SUCC six ;;
let eight = SUCC seven ;;
let nine = SUCC eight ;;
*)
