type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
	| Poland | Portugal | Italy | Germany | Sweden | England
	| Croatia | Argentina
type tourna = LEAF of team
	| NODE of tourna * tourna

exception Error

let rec height tourna = (* measure the tree's height *)
	match tourna with
		NODE(x, y) -> 
		(
		 	if height(x) > height(y) then height(x) + 1
			else height(y)+1
		)
		| LEAF x -> 0

exception NotImplemented
let rec power(a, n) =
	if n>1 then a * power(a,n-1)
	else if n=1 then a
	else if n=0 then 1
	else raise NotImplemented

let an n =
	power(2, n) - 1

let rec makeStringList (tourna, h) =
	makeStringList2 (tourna, h, h, [])

and makeStringList2 (tourna, h, f, l) =
	if h=0 then "|"::l
	else if f=h then makeStringList2(tourna, h, f-1, (makeSequence(" ", an(f))^"|")::l)
	else if f = 0 then makeStringFloor(tourna, h, f, "")::l
	else makeStringList2(tourna, h, f-1, makeStringFloor(tourna, h, f, "")::l)
	
and makeStringFloor (tourna, h, f, s) =
(*	if f=h then makeSequence(" ", an(h))^"|"
	else*)
		( match tourna with
			LEAF x -> s^makeSequence(" ", an(f+1))^"|"^makeSequence(" ", an(f+1))^" "
			| NODE (x, y) ->
				if f=(h-1) then s^makeSequence(" ", an(f))^"|"^makeSequence("-", an(f+1))^"|"^makeSequence(" ", an(f))^" "
				else ( match (x,y) with
					(LEAF p, LEAF q) -> s^makeSequence(" ", an(h-1))^"|"^makeSequence(" ", an(h))^"|"^makeSequence(" ", an(h-1))^" "
					| (LEAF p, _) -> makeStringFloor(y, h-1, f, s^makeSequence(" ", an(h-1))^"|"^makeSequence(" ",an(h-1))^" ")
					| (_, LEAF q) -> (makeStringFloor(x, h-1, f, s))^makeSequence(" ", an(h-1))^"|"^makeSequence(" ",an(h-1))^" "
					| _ -> s^(makeStringFloor(x, h-1, f, ""))^(makeStringFloor(y, h-1, f, ""))
				)
		)

and makeSequence(s, n) = (* s string을 n번만큼 반복한 string을 만들어준다. *)
	if n>1 then s^makeSequence(s, n-1)
	else if n=1 then s
	else if n=0 then ""
	else raise Error

let rec printList(l) = 
	match l with
		s::[] -> print_string(s); print_newline()
		| s::l2 -> print_string(s); print_newline(); printList(l2)
		| [] -> ()
	
let pptree tourna =
	printList(List.rev (makeStringList(tourna, height(tourna))))

(*
let rec makeList tourna = (* make teamList *)
	makeList2 ([], tourna)
(*	match tourna with
		NODE(x, y) -> ( match (x,y) with
			(LEAF p, LEAF q) ->
			| (LEAF p, NODE(a, b)) ->
			| (NODE(a, b), LEAF q) ->
			| _ -> 
		)
		| LEAF x -> 
		*)
and makeList2 (teamList, tourna) =
	match tourna with
		NODE(x, y) -> ( match (x,y) with
			(LEAF p, LEAF q) -> MATCH(p,q)::teamList
			| (LEAF p, NODE(a, b)) -> makeList2(SINGLE(p)::teamList, NODE(a,b))
			| (NODE(a, b), LEAF q) -> SINGLE(q)::(makeList2(teamList, NODE(a, b)))
			| _ -> (makeList(y)@makeList(x))@teamList
		)
		| LEAF x -> (* 대진표에 팀이 하나만 있는 경우 *) SINGLE(x)::teamList
*)

(*
let rec pptree tourna = 
	let l = makeList(tourna) and h = height(tourna) in 
		
and printLEAF (teamList, height) =
and printNODE (height, floor) =
	if floor < height then print_newline()
	*)
