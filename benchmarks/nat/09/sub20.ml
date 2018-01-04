(* C:\Documents and Settings\Administrator\¹ÙÅÁ È­¸é\pl_first_homework\Exercise5.ml *)

(* Computer S&E/2007-15612/park sungjun*)
(*Exercise 5*)
type nat = ZERO | SUCC of nat 

let rec natadd (m,n)=
match m with
|ZERO -> n
|SUCC a -> SUCC ( natadd(a,n)) 

let rec natmul (a,b)=
match b with
|ZERO -> ZERO
|SUCC n -> natadd ( (natmul (a,n)),a) ;;

