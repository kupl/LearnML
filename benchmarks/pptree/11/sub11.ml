(* HW 2-1 / 2007-11603 / 컴퓨터공학부 / 이영준 *)

type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
| Poland | Portugal | Italy | Germany | Sweden | England
| Croatia | Argentina
type tourna = LEAF of team
| NODE of tourna * tourna

(* n개의 spaces/bars를 string화하여 리턴. 최초에는 str에 "" 전달! *)
let rec makeSubStr n c str =
	if n > 0 then
		(makeSubStr (n-1) c (c ^ str))
	else 
		str

(* 두 정수 중 큰 수 리턴 *)
let maxInt a b = if a >= b then a else b

(* 트리의 높이 계산. 최초 height는 0 *)
let rec findHeight (t, height) =
	match t with LEAF _ -> (height + 1)
				  | NODE(a,b) -> (maxInt (findHeight (a, (height+1))) (findHeight (b, (height+1))))

let rec baseStr (t, idx, lst) =
	let rec inner t idx lst =
		match t with
		| LEAF _ -> ("|", idx::lst)
		| NODE(x,y) -> ( match x with LEAF _ -> ( match y with LEAF _ -> ("|-|", (idx+1)::lst)
																			|  NODE(_,_) -> ( let (s,l) = (inner y (idx+4) ((idx+1)::lst)) in
																									(" |  " ^ s, l) ) )
											|  NODE(_,_) -> (match y with LEAF _ -> ( let (s,l) = (inner x idx lst) in
																									(s ^ "  | ", ((String.length s)+2)::l) )
																				|  NODE(_,_) -> ( let (s1, l1) = (inner x idx lst) in
																										let (s2, l2) = (inner y ((String.length s1)+idx+1) l1) in
																										(s1^" "^s2, l2) ) ) ) 
	in

	let rec trimFront str lst =
		if (String.get str 0) = ' ' then
			(trimFront (String.sub str 1 ((String.length str)-1)) (List.map (function x -> x-1) lst))
		else
			(str, lst)
	in

	let rec trimBack str = 
		if (String.get str ((String.length str)-1)) = ' ' then
			(trimBack (String.sub str 0 ((String.length str)-1)))
		else
			str
	in
	
	let (s, l) = (inner t idx lst) in
	let (s, l) = (trimFront s l) in
	let s = (trimBack s) in
	
	(s, l)


(* height를 같은 트리로 t를 줄여줌 *)
let rec compactTree (t, cur, height) = 
	match t with
	| LEAF x -> (LEAF x)
	| NODE(a,b) -> if cur = height then
							(LEAF Korea)
						else 
							(NODE ((compactTree (a, (cur+1), height)), (compactTree (b, (cur+1), height))))

(* 기존 str에 새로운 c를 idx 자리에 붙이기
   이때 idx > String.length일 경우 subStr을 붙인다. *)
let rec insertChar c sub_c str idx = 
	let subStr = (makeSubStr (idx - (String.length str)) sub_c "") in
	(str ^ subStr ^ c)


let rec otherStr t str idxList idxNum newIdxList = 
	match t with
	| LEAF _ -> ( let idx = (List.nth idxList idxNum) in
				     ((insertChar "|" " " str idx), (idx::newIdxList), (idxNum+1)) )
	| NODE(a,b) -> ( match a with 
							| LEAF _ -> ( match b with LEAF _ -> ( let s1 = (insertChar "|" " " str (List.nth idxList idxNum)) in
																				let s2 = (insertChar "|" "-" s1 (List.nth idxList (idxNum+1))) in
																				(s2, ((((String.length s1)+(List.nth idxList (idxNum+1)))/2)::newIdxList), (idxNum+2)) )
															 | NODE(_,_) -> ( let s1 = (insertChar "|" " " str (List.nth idxList idxNum)) in
																					(otherStr b s1 idxList (idxNum+1) ((List.nth idxList idxNum)::newIdxList)) ) )
							| NODE(_,_) -> ( match b with LEAF _ -> ( let (s1,l,n) = (otherStr a str idxList idxNum newIdxList) in
																					let s2 = (insertChar "|" " " s1 (List.nth idxList n)) in
																					(s2, ((List.nth idxList n)::l), (n+1)) )
																 | NODE(_,_) -> ( let (s1,l1,n1) = (otherStr a str idxList idxNum newIdxList) in
																						(otherStr b s1 idxList n1 l1) ) ) )

																				

let rec makeStr t lst lvl height idxList = 
	if lvl = height then
		let (base, l) = (baseStr (t, 0, [])) in
		(makeStr t (base::lst) (lvl-1) height (List.rev l))
	else if lvl = 0 then
		lst
	else
		let newT = (compactTree (t, 1, (height-1))) in
		let (s, l, n_null) = (otherStr newT "" idxList 0 (0::[])) in 
		(makeStr t (s::lst) (lvl-1) height (List.rev l))

let rec printStrList lst =
	match lst with 
	| [] -> (print_string "")
	| h::t -> ( print_string h;
					print_newline(); 
					(printStrList t) )


			
let pptree t =
	let height = (findHeight (t, 0)) in
	(printStrList (makeStr t [] height height []))