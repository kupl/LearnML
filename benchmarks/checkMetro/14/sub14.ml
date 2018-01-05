(* 프로그래밍언어 HW1 Exercise 2
   2009-11657 김동현 *)
	
type metro = STATION of name
           | AREA of name * metro
					 | CONNECT of metro * metro
and name = string

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

let rec checkMetro2 (met, lst) =
	match met with
	| STATION s -> searchList (s, lst)
	(* input이 STATION "s"이면 false 리턴 *)
	| CONNECT (m1, m2) -> checkMetro2 (m1, lst) && checkMetro2 (m2, lst)
	(* input이 CONNECT (m1, m2)이면 m1, m2가 각각 조건을 만족하는지 체크 *)
	| AREA (a, STATION s) -> searchList (s, a::lst)
	| AREA (a, AREA (b, m)) -> checkMetro2 (AREA (b, m), a::lst)
	| AREA (a, CONNECT (m1, m2)) -> checkMetro2 (m1, a::lst) && checkMetro2 (m2, a::lst)
  
let checkMetro met = checkMetro2 (met, [])