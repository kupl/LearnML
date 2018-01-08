type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal | Italy | Germany | Sweden | England | Croatia | Argentina
type tourna = LEAF of team
	|NODE of tourna * tourna

let test = NODE (NODE (LEAF Korea, LEAF Portugal), LEAF Brazil)

let pptree tour =

	let rec func n = 
		if n<=1 then 0
		else (int_of_float ((float_of_int 2) ** (float_of_int (n-2))))+(func (n-1))
	in

	let makeblank n =
		let rec makeblanktmp k =
			if k<=0 then ""
			else " "^(makeblanktmp (k-1))
		in
	(makeblanktmp (func n))
	in

	let makeline n = 
		let rec makelinetmp k =
			if k<=0 then ""
			else "-"^(makelinetmp (k-1))
		in
	(makelinetmp (func n))
	in

	let rec concatlst lst1 lst2 =
		if lst1 = [] then lst2
		else if lst2 = [] then lst1
		else ((List.hd lst1)^" "^(List.hd lst2))::(concatlst (List.tl lst1) (List.tl lst2))
	in

	let rec preconcat a lst =
		match lst with
		[]->[]
		|h::t -> (a^" "^h)::(preconcat a t)
	in

	let rec postconcat a lst =
		match lst with
		[]->[]
		|h::t -> (h^" "^a)::(postconcat a t)
	in
	
	let rec maketree t n =
		match t with
		LEAF a -> [(makeblank n)^"|"^(makeblank n)]
		|NODE (LEAF a, b) -> [(makeblank n)^"|"^(makeblank n);(makeblank (n-1))^"|"^(makeline n)^"|"^(makeblank (n-1))]@(preconcat (List.hd (maketree (LEAF a) (n-1))) (concatlst [] (List.tl (maketree b (n-1)))))
		|NODE (a, LEAF b) -> [(makeblank n)^"|"^(makeblank n);(makeblank (n-1))^"|"^(makeline n)^"|"^(makeblank (n-1))]@(postconcat (List.hd (maketree (LEAF b) (n-1))) (concatlst (List.tl (maketree a (n-1))) []))
		|NODE (a, b) -> [(makeblank n)^"|"^(makeblank n);(makeblank (n-1))^"|"^(makeline n)^"|"^(makeblank (n-1))]@(concatlst (List.tl (maketree a (n-1))) (List.tl (maketree b (n-1))))
	in

	let rec howlayer t =
		let max a b = if a>=b then a else b in
		match t with
		LEAF a -> 1
		|NODE (a, b) -> (max (howlayer a) (howlayer b)) + 1
	in

	let treelst t = (maketree t (howlayer t)) in

	let rec concatstr lst =
		match lst with
		[]->""
		|h::t -> h^"\n"^(concatstr t)
	in

	print_string (concatstr (treelst tour))
