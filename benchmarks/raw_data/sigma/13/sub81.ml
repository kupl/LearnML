(* file name : ex1.ml *)
(* author : Jisoon Park (jspark@ropas.snu.ac.kr) *)
(* date : 2013-09-13 *)
(* Exercise 1 *)
let rec sigma : int * int * (int->int) -> int
 = fun (a, b, f) -> 
	if (a > b) then 0
	else if (a = b) then f a
	else (f a) + sigma(a + 1, b, f)
