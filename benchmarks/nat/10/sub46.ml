(* C:\Users\owner\Desktop\Homework 1(6) *)

type nat = ZERO | SUCC of nat ;;

let rec natadd (a,b) =
	match a with
  	ZERO -> b
  	| SUCC(nata) -> natadd (nata, SUCC(b)) ;;

