(* C:\Documents and Settings\Administrator\���� ȭ��\pl_first_homework\Exercise1.ml *)

(* Computer S&E/2007-15612/park sungjun*)
(* Exercise 1*)
exception Error of string 

let rec sigma f a b =
	if a=b then (f b)
	else if a>b then raise ( Error "Illegal input")
  		else (sigma f (a+1) b + (f a))
  ;;

