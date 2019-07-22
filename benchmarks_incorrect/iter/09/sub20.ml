(* C:\Documents and Settings\Administrator\πŸ≈¡ »≠∏È\pl_first_homework\Exercise2.ml *)

(* Computer S&E/2007-15612/park sungjun*)
(*Exercise 2*)
exception Error of string 

let rec iter (n,f) x =
if n<0 then raise ( Error "Illegal input")
else
match n with
|0->0
|1->f x
|_->f (iter (n-1,f) x)
;;

