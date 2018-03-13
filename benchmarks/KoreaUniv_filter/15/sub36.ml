(* 2012210066 hw2.ml
	  	 컴퓨터학과 조현상 *)

(* Problem 1: filter *)
let rec filter f l = 
match l with
| [] -> []
| h::t -> if f h then h::filter f t else filter f t;;
