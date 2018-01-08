type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal | Italy | Germany | Sweden | England | Croatia | Argentina | A | B
type tourna = LEAF of team | NODE of tourna * tourna
type item = tourna * int * int
(* tourna, depth, position *)

let a0 = NODE(NODE(LEAF A, LEAF B), LEAF B)

let a = NODE(NODE(NODE(NODE(LEAF A, LEAF B), NODE(LEAF A, LEAF B)), LEAF B), NODE(LEAF A, LEAF B))
let b = NODE(NODE(NODE(NODE(LEAF A, LEAF B), NODE(LEAF A, LEAF B)), LEAF B), NODE (NODE (LEAF A, LEAF B), LEAF B)) 
let c = NODE(LEAF A, NODE(NODE(NODE(NODE(LEAF A, LEAF B), NODE(LEAF A, LEAF B)), LEAF B), NODE (NODE (LEAF A, LEAF B), LEAF B)))

let getMax(a, b) =
	if a>b then a
	else b

let rec getDepth(_tourna, depth) =
	match _tourna with
	LEAF(_team) -> depth
	| NODE(_t1, _t2) -> 
		getMax(getDepth(_t1, depth+1), getDepth(_t2, depth+1))

let getNewline(depth, _queue) =
	match _queue with
	[] -> ""
	| (_tourna,d,p)::t -> (if depth=d then ""
							else "\n")

let rec getWidth depth =
	if depth<=1 then 1
	else ((getWidth (depth-1))*2+1)


let rec bfsPrint (_queue, _string) =
	let spaceDone = ((String.length _string) - (String.rindex _string '\n')) in
	let makeSpace n = if n<0 then 0 else n in
	match _queue with
	[] -> _string
	| (LEAF(_team), 0, position)::t -> _string
	| (LEAF(_team), depth, position)::t -> 
		bfsPrint(
				t@[(LEAF(_team), depth-1, position)], 
				(_string^(String.make ((makeSpace (position-spaceDone))+1) ' ')^"|"^getNewline(depth,t)))
	| (NODE(_t1, _t2), 0, position)::t -> _string
	| (NODE(_t1, _t2), depth, position)::t -> 
		bfsPrint(
				t@[(_t1, depth-1, position-getWidth(depth)-1)]@[(_t2, depth-1, position+getWidth(depth)+1)],
				(_string^(String.make (makeSpace ((position-getWidth(depth)) - spaceDone)) ' ')^"|"^(String.make (getWidth(depth+1)) '-')^"|")^(getNewline(depth,
				t@[(_t1, depth-1, position-getWidth(depth)-1)]@[(_t2, depth-1, position+getWidth(depth)+1)])))


let pptree _tourna =
	let depth = getDepth(_tourna, 0) in
	let firstLine =  ((String.make (getWidth(depth+1)) ' ')^"|"^"\n") in
	match _tourna with
	LEAF(_team) -> print_string "|"
	| _ -> print_string (bfsPrint([(_tourna, depth, (getWidth(depth+1)))], firstLine))
