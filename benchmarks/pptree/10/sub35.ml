type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal | Italy | Germany | Sweden | England | Croatia | Argentina
type tourna = LEAF of team
	| NODE of tourna * tourna
;;

let pptree tour =
	let rec count t = match t with
		LEAF a -> 0
		| NODE (a,b) -> if (count a) > (count b) then (count a) + 1
				else (count b) + 1
	in
	let mkstr a b = 
		let rec tail c d = match (c,d) with
			|(h1::t1,h2::t2) -> [h1^" "^h2]@(tail t1 t2)
			| _ -> []
		in
		match (a,b) with
		(h1::t1,h2::t2) -> [h1^"-"^h2]@(tail t1 t2)
		| _ -> []
	in
	let rec dou n =
		if n = 1 then 1
		else ((dou (n - 1)) * 2) + 1
	in
	let bar n =
		let rec pbar k =
			if k = 1 then "-"
				else "-"^(pbar (k-1))	in
		pbar (dou n)
	in
	let spa n =
		let rec pspa k =
			if k = 1 then " "
				else " "^(pspa (k-1))	in
		pspa (dou n)
	in
	let rec pp t n m = 
		let rec leaf a k = if k < 0 then []
				else [(spa n)^"|"^(spa n)]@(leaf a (k-1))
		in
		match t with
		LEAF a -> if n = 0 then ["|"]
			else ( match m with
				1 -> [(spa n)^"|"^(bar n)]@(leaf a (n - 1))
				|2 -> [(bar n)^"|"^(spa n)]@(leaf a (n - 1))
				|3 -> [(spa n)^"|"^(spa n)]@(leaf a (n - 1))
				| _ -> [])
		| NODE (a,b) -> (if m = 3 then [(spa n)^"|"^(spa n)]
				else (if m = 1 then [(spa n)^"|"^(bar n)]
					else [(bar n)^"|"^(spa n)]))
				@(mkstr (pp a (n-1) 1) (pp b (n-1) 2))
	in
	let rec prt lst = match lst with
		h::t -> print_string (h^"\n");(prt t)
		| [] -> print_string ""
	in
	prt (pp tour (count tour) 3)

;;
