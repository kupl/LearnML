(* 2009-11679 김정명 2-1 *)

type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon |
			Poland | Portugal | Italy | Germany | Sweden | England
		  | Croatia | Argentina
type tourna = LEAF of team
			| NODE of tourna * tourna

	let rec replace_elem (lst, elem, nth) =
		if nth = 0 then elem :: (List.tl lst) else
		match lst with 
		  [] -> []
		| h::t -> h::( replace_elem (t, elem, nth-1) )
	;;
	(* string 을 다른 string에 넣는다. *)
	let rec replace_string (loc, n, base, str) =
		if n = 0 then base
		else (base.[loc + n - 1] <- str.[n-1];
			 replace_string (loc, n-1, base, str))
	;;
let rec pptree tour =
	(* 깊이 : 0 부터 시작 *)
	let rec depth t=
		match t with
		  LEAF l -> 0
		| NODE (t1, t2) -> if (depth t1 > depth t2) then (depth t1) + 1
						   else (depth t2) + 1
	in
	(* 기준이 되는 수 *)
	let rec magic n =
		if n = 0 then 0
		else (magic (n-1) * 2 + 1)
	in
	(* n개의 space *)
	let rec space n =
		if n = 0 then ""
		else (space (n-1) ^ " ")
	in
	(* n개의 hyphon *)
	let rec hyphon n = 
		if n = 0 then ""
		else if n = 1 then "-"
		else ("-" ^ hyphon (n-1))
	in
	(* n층에서 leaf일때 *)
	let middle n = 
		(space ((magic n)+1)) ^ "|" ^ (space ((magic n)+1))
	in
	(* n층에서 leaf가 아닐때 *)
	let full n = 
		"|" ^ (hyphon (magic (n+1))) ^ "|"
	in
	(* d depth tree 에서 n번째 층. cnt는 항상 magic (d - n)부터 시작 -> floor2 를 도와줌 *)
	let rec floor (n, cnt) =
		if cnt = 0 then ""
		else if cnt = 1 then full n
		else if ((cnt mod 2) = 1) then ((full n) ^ floor (n, cnt -1))
		else (space (magic (n+1)) ^ floor (n, cnt -1))
	in
	(* d depth tree 에서 n번째 층 (0부터 아래기준) *)
	let floor2 (n, d) = 
		if n = d then (space (magic n)) ^ "|"
		else (space (magic n)) ^ floor (n, magic (d - n))
	in	
	(* depth + 1 만큼의 list를 층별로 만든다 ex) 2이면 ["   |   ";" |---|";"|-| |-|"]*)
	let rec init (d, cnt, lst) =
		if cnt = -1 then lst
		else init (d, cnt-1, lst @ [floor2 (cnt, d)])
	in
	(* base 의 n승 *)
	let rec power base n =
		if n = 0 then 1
		else (power base (n-1)) * base
	in
	(* repair 용 함수 *)
	let rec replace (d, cnt, number, lst, special) =
		let ang = 
		replace_elem (lst,
					  replace_string (((magic (d-(cnt+1))) + (String.length (floor (d-(cnt+1), (number-(power 2 cnt)) * 2 ))) - magic (special - cnt)), 
										String.length (middle (d-(cnt+1))) + (magic (special -cnt) * 2), 
										(List.nth lst (special+1)), 
										space (magic (special - cnt)) ^ middle (d-(cnt+1)) ^ space (magic (special-cnt)) ), 
					  special + 1)
		in
		if special = (d-1) then ang
		else replace (d, cnt, number, ang, special + 1)
	in
	(* d depth 트리. cnt는 0부터 시작을 한다. number 는 1부터 *)
	let rec repair (t, d, cnt, number, lst) =
		match t with
		  LEAF l -> if cnt = d then lst
					else replace (d, cnt, number, lst, cnt)
		| NODE (t1, t2) -> repair (t2, d, cnt + 1, number * 2 + 1, repair (t1, d, cnt + 1, number * 2, lst))
	in
	(* string 리스트를 트리모양으로 출력하도록 string을 합친다. *)
	let rec construct lst =
		match lst with
		  [] -> ""
		| h::t -> h ^ "\n" ^ (construct t)
	in
	let tourna = (init (depth tour, depth tour, [])) in
	print_string (construct (repair (tour, depth tour, 0, 1, tourna)))
;;
