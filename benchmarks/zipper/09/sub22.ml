(* C:\Documents and Settings\Administrator\¹ÙÅÁ È­¸é\pl_first_homework\Exercise3.ml *)

(* Computer S&E/2007-15612/park sungjun*)
(*Exercise 3*)
let rec zipper (lista, listb)=
match (lista, listb) with
|([],[]) -> []
|(h::t,[]) -> h::t
|([],h::t) -> h::t
|(h1::t1,h2::t2) -> [h1]@[h2]@zipper(t1,t2)
;;

