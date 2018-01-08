type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
		  | Poland | Portugal | Italy | Germany | Sweden | England
		  | Croatia | Argentina
type tourna = LEAF of team
			| NODE of tourna * tourna

exception Error of string

let rec make_line n item =
	match n with
	  0 -> ""
	| k when n > 0 -> item^(make_line (k-1) item)
	| _ -> raise (Error "negative input number in make_line")

let merge_head ele1 ele2 =
	let index1 = String.index ele1 '|' in
	let index2 = String.index ele2 '|' in
	let len =  (((String.length ele1)-index1)+index2) in
	(String.sub ele1 0 (index1+1))^
	(make_line len "-")^
	(String.sub ele2 index2 ((String.length ele2)-index2))

let rec merge_tail lst1 lst2 =
	match (lst1, lst2) with
	  ([], []) -> []
	| (h1::t1, h2::t2) -> (h1^" "^h2)::(merge_tail t1 t2)
	| _ -> raise (Error "two list has different length in merge_tail")

let rec add_blank strlst =
	match strlst with
	  [] -> []
	| h::t -> (h^" ")::(add_blank t)

let merge strlst1 strlst2 =
	let ele1 = List.hd strlst1 in
	let ele2 = List.hd strlst2 in
	let len =  ((String.length ele1)-(String.index ele1 '|'))+(String.index ele2 '|') in
	let is_odd = len/2 == (len-1)/2 in
	let make_odd =
		if is_odd then (strlst1, strlst2) else (add_blank strlst1, strlst2)
	in
	match make_odd with
	  ([], _) | (_, []) -> raise (Error "empty string list in merge")
	| (h1::t1, h2::t2) -> (merge_head h1 h2)::(merge_tail t1 t2)

let rec up strlst n =
	match n with
	  0 -> strlst
	| _ when n > 0 -> up ((List.hd strlst)::strlst) (n-1)
	| _ -> raise (Error "negative n in up")

let new_root strlst =
	let head = List.hd strlst in
	let index = String.index head '|' in
	let len = (String.rindex head '|') - index + 1 in
	let res = make_line (String.length head) " " in
	let _ = String.set res (index + len/2) '|' in
	res::strlst

let rec ptree_strlst tree =
	match tree with
	  NODE (LEAF _, LEAF _) -> [" | "; "|-|"]
	| LEAF _ -> [" | "]
	| NODE (t1, t2) ->
		let strlst1 = ptree_strlst t1 in
		let strlst2 = ptree_strlst t2 in
		let diff = (List.length strlst1) - (List.length strlst2) in
		(match diff with
		   0 -> (new_root (merge strlst1 strlst2))
		 | _ when diff > 0 -> (new_root (merge strlst1 (up strlst2 diff)))
		 | _ -> (new_root (merge (up strlst1 (-1*diff)) strlst2))
		)

let rec ptree_str strlst =
	match strlst with
	  [] -> ""
	| h::t -> h ^ "\n" ^ (ptree_str t)

let pptree tree = print_string (ptree_str (ptree_strlst tree))

let s =  NODE (NODE (NODE (LEAF Korea, LEAF Korea), NODE (LEAF Korea, LEAF Korea)),
			   NODE (LEAF Korea, LEAF Korea)) 
let t = NODE(LEAF Korea, LEAF Korea)
let ss = NODE ( s , NODE(s, t))
