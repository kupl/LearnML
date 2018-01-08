type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal | Italy | Germany | Sweden | England | Croatia | Argentina

type tourna = LEAF of team
	| NODE of tourna * tourna

let toString t =
	match t
	with Korea -> "Korea"
	| France -> "France"
	| Usa -> "Usa"
	| Brazil -> "Brazil"
	| Japan -> "Japan"
	| Nigeria -> "Nigeria"
	| Cameroon -> "Cameroon"
	| Poland -> "Poland"
	| Portugal -> "Portugal"
	| Italy -> "Italy"
	| Germany -> "Germany"
	| Sweden -> "Sweden"
	| England -> "England"
	| Croatia -> "Croatia"
	| Argentina -> "Argentina"

let rec parenize tree =
	match tree
	with LEAF tm -> (toString tm)
	| NODE (tr1, tr2) -> "("^(parenize tr1)^" "^(parenize tr2)^")"

exception InternalError

let getBigger int1 int2 = if int1>int2 then int1 else int2

let rec width depth =
	if depth=0 then 0 else 2*(width (depth-1))+1

let rec space n =
	if n=0 then "" else " "^(space (n-1))

let rec hyphen n =
	if n=0 then "" else "-"^(hyphen (n-1))

let rec copy this n =
	if n=0 then [] else (this::(copy this (n-1)))

let concatSpace str1 str2 = (str1^" "^str2)
let concatNospace str1 str2 = (str1^str2)

let printOut dig = (List.iter print_endline dig)

let eqMerge depth subDig1 subDig2 =
let w = (width depth) in
let wm = (width (depth-1)) in
let top =
	[          (space w)^"|"^(space w) ;
	  (space wm)^"|"^(hyphen w)^"|"^(space wm) ] in
let bottom = match (subDig1,subDig2) with
	  ((head1::rest1),(head2::rest2)) -> (List.map2 concatSpace rest1 rest2)
	| _ -> raise InternalError
in (
	top@bottom
)

let merge (depth1,subDig1) (depth2,subDig2) =
let subDig1 =
	if depth1>=depth2 then subDig1 else
	let diff = depth2-depth1 in
	let w = (width (depth2-1)) in
	let wm = (width (depth1-1)) in
	let top = (copy ((space w)^"|"^(space w)) diff) in
	let bottom = (List.map2 concatNospace (copy (space (w-wm)) depth1) subDig1) in
	let bottom = (List.map2 concatNospace bottom (copy (space (w-wm)) depth1))
	in (
		top@bottom
	)
in let subDig2 =
	if depth1<=depth2 then subDig2 else
	let diff = depth1-depth2 in
	let w = (width (depth1-1)) in
	let wm = (width (depth2-1)) in
	let top = (copy ((space w)^"|"^(space w)) diff) in
	let bottom = (List.map2 concatNospace (copy (space (w-wm)) depth2) subDig2) in
	let bottom = (List.map2 concatNospace bottom (copy (space (w-wm)) depth2))
	in (
		top@bottom
	)
in let depth = (getBigger depth1 depth2)
in (
	eqMerge depth subDig1 subDig2
)	


let rec getDepth targetTree = match targetTree with
	  LEAF _ -> 1
	| NODE (subTree1, subTree2) -> 1 + (getBigger (getDepth subTree1) (getDepth subTree2))

let rec getDig targetTree = match targetTree with
	  LEAF _ -> ["|"]
	| NODE (subTree1, subTree2) ->
		let depth1 = getDepth subTree1 in
		let depth2 = getDepth subTree2 in
		let subDig1 = getDig subTree1 in
		let subDig2 = getDig subTree2
		in (
			merge (depth1, subDig1) (depth2,subDig2)
		)

let pptree targetTree = printOut (getDig targetTree)
