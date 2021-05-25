(* C:\Documents and Settings\Administrator\¹ÙÅÁ È­¸é\pl_first_homework\Exercise1.ml *)

(* Computer S&E/2007-15612/park sungjun*)
(* Exercise 1*)
exception Error of string 

let rec sigma (a,b,f)=
	if a=b then (f b)
	else if a>b then raise ( Error "Illegal input")
  		else (sigma (a+1, b, f) + (f a))
  ;;

