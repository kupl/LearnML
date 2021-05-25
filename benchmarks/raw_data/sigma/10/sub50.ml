(* C:\Users\owner\Desktop\Homework 1(1).ml *)

exception Error of string;;

let rec sigma(a, b, f) =
  	if (a == b) then (f a)
  	else if (a < b) then (f a) + sigma((a+1), b, f)
  	else raise (Error "FAIL!") ;;

