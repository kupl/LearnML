(* 프로그래밍언어 HW1 Exercise 2
   2009-11657 김동현 *)
	
type lambda = V of var
           | P of var * lambda
					 | C of lambda * lambda
and var = string

(*
let rec searchList (elem, lst) =
	if lst = [] then false
	else
	  let (h::t) = lst in
	  if elem = h then true
	  else searchList (elem, t)
*)

let rec searchList (elem, lst) =
	match lst with
	| [] -> false
	| h::t -> if elem = h then true
	          else searchList (elem, t)
(* 리스트 내에 원소가 존재하면 true, 없으면 false return *)

let rec check2 (met, lst) =
	match met with
	| V s -> searchList (s, lst)
	(* input이 V "s"이면 false 리턴 *)
	| C (m1, m2) -> check2 (m1, lst) && check2 (m2, lst)
	(* input이 C (m1, m2)이면 m1, m2가 각각 조건을 만족하는지 체크 *)
	| P (a, V s) -> searchList (s, a::lst)
	| P (a, P (b, m)) -> check2 (P (b, m), a::lst)
	| P (a, C (m1, m2)) -> check2 (m1, a::lst) && check2 (m2, a::lst)
  
let check met = check2 (met, [])