(* file name : ex1.ml *)
(* author : Jisoon Park (jspark@ropas.snu.ac.kr) *)
(* date : 2013-09-13 *)
(* Exercise 1 *)
let rec sigma f a b =
	if (a > b) then 0
	else if (a = b) then f a
	else (f a) + sigma f (a+1) b
