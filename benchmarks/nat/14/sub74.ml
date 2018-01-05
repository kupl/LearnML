(*
 * Brief      : HW1, Program Language (4190.310)
 * Author     : YongKi Kim <kim.yongki@ropas.snu.ac.kr>
 * Student Id : 2014-21767
 * Date       : Sep. 12, 2014
 *)

(* Exercise 3 *)
type nat = ZERO | SUCC of nat

let rec eval_nat : nat -> int = function
| ZERO   -> 0
| SUCC n -> 1 + (eval_nat n)

let rec trans_nat : int -> nat = function
| 0 -> ZERO
| n -> SUCC (trans_nat (n-1))

let natadd : nat * nat -> nat = fun (n1,n2) ->
	trans_nat ((eval_nat n1) + (eval_nat n2))

let natmul : nat * nat -> nat = fun (n1,n2) ->
	trans_nat ((eval_nat n1) * (eval_nat n2))
