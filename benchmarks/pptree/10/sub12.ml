type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
| Poland | Portugal | Italy | Germany | Sweden | England
| Croatia | Argentina
type tourna = LEAF of team
| NODE of tourna * tourna

let rec cntbar tourna =
	match tourna with
		LEAF t -> 0
		| NODE (t1, t2) -> 
			if (cntbar t1)>(cntbar t2) then ((cntbar t1)*2+1)
			else ((cntbar t2)*2+1);;
let rec cntdeep tourna =
	match tourna with
		LEAF t -> 1
		| NODE (t1, t2) ->
			if (cntdeep t1)>(cntdeep t2) then (cntdeep t1)+1
			else (cntdeep t2)+1;;
		
let rec mkbarlist (v, a, b) = 
	if a=b then [(v, b)] 
	else ((v, a)::(mkbarlist (v, a+1, b)));;
exception Wrong;;
let rec mkwidth (tournalist, parentlist, barlist) =
	match tournalist with
		tourna::tl1 -> 
			(match tourna with
			 	LEAF team -> 
					(match parentlist with
					 	(v, w)::tl2 -> (match (mkwidth (tl1, tl2, barlist)) with (plist, blist) -> ((v+1, w)::plist, blist))
						| [] -> raise Wrong
					)
				| NODE (t1, t2) ->
					(match parentlist with
					 	(v, w)::tl2 ->
											(match (mkwidth (tl1, tl2, (mkbarlist (v+1, w-((cntbar tourna)-1)/2,  w+((cntbar tourna)-1)/2))@barlist)) with
											 	(plist, blist) ->  ((v+1, w-((cntbar tourna)+1)/2)::(v+1, w+((cntbar tourna)+1)/2)::plist, blist))
						| [] -> raise Wrong
					)
			)
		| [] -> ([], barlist);;
let rec isNode tournalist =
	match tournalist with
		(LEAF team)::tl -> isNode tl
		| (NODE (t1, t2))::tl -> true
		| [] -> false;;
let rec mktournalist tournalist =
	match tournalist with
		(LEAF team)::tl -> (LEAF team)::(mktournalist tl)
		| (NODE (t1, t2))::tl -> t1::t2::(mktournalist tl)
		| [] -> [];;
let rec mkparents (tournalist, parentlist) =
	match (mkwidth (tournalist, parentlist, [])) with
		(plist, blist) ->
			(if isNode tournalist then parentlist@(mkparents ((mktournalist tournalist), plist))
			else parentlist@plist
			);;
let rec mkbars (tournalist, parentlist) =
	match (mkwidth (tournalist, parentlist, [])) with
		(plist, blist) ->
			(if isNode tournalist then blist@(mkbars ((mktournalist tournalist), plist))
			 else blist
			);;
let rec mktree (tourna, v,w, deep, last) =
	if w > last then 
		(if v=deep then "\n" 
		else "\n"^(mktree (tourna, v+1, 1,deep, last)))
	else
		if List.mem (v, w) (mkbars ([tourna], [(1, (cntbar tourna)+1)])) then
			"-"^(mktree (tourna, v, w+1,deep,  last))
		else
		if List.mem (v, w) (mkparents ([tourna], [(1, (cntbar tourna)+1)])) then 
			"|"^(mktree (tourna, v, w+1,deep,  last))
		else
			" "^(mktree (tourna, v, w+1,deep,  last))

let pptree tourna =
	print_string (mktree (tourna, 1, 1, cntdeep tourna, (cntbar tourna)*2 +1));;

(*
	let a = LEAF Korea 
	let b = LEAF Japan 
	let c = NODE (a, b) 
	let d = NODE (c, a) 
	let e = NODE (b, c) 
	let f = NODE (c, c) 
	let g = NODE (d, a) 
	let h = NODE (a, d) 
	let i = NODE (a, g) 
	let j = NODE (h, d) 
	let k = NODE (i, j) 
	let l = NODE (c, c) 
	let m = NODE (l, l) 
	let n = NODE (m, m) 
	let o = NODE (n, n) 
	let p = NODE (o, o) ;;
	*)
