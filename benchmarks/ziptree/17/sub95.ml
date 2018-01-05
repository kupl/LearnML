type item = string
type tree = LEAF of item
					| NODE of tree list

type zipper = TOP
						| HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

exception NOMOVE of string

let goLeft loc = 
	match loc with
	| LOC (t, TOP) -> raise (NOMOVE "left of top")
	| LOC (t, HAND(l :: left, up, right)) -> 
		LOC (l, HAND(left, up, t :: right))
	| LOC (t, HAND([], up, right)) ->
		raise (NOMOVE "left of first")

let goRight loc =
	match loc with
	| LOC (t, TOP) -> raise (NOMOVE "right of top")
	| LOC (t, HAND(left, up, r :: right)) ->
		LOC (r, HAND(t :: left, up, right))
	| LOC (t, HAND(left, up, [])) ->
		raise (NOMOVE "right of first")

let goUp loc =
	let rec newlist list1 list2 =
		match list1 with
		| [] -> list2
		| h :: t -> newlist t (h::list2) in
	match loc with
	| LOC (t, TOP) -> raise (NOMOVE "up of top")
	| LOC (t, HAND(left, TOP, right)) ->
		LOC (NODE(newlist left (t::right)), TOP)
	| LOC (t, HAND(left, HAND(l, up, r), right)) ->
		LOC (NODE(newlist left (t::right)), HAND(l, up, r))

let goDown loc =
	match loc with
	| LOC (NODE(h::t), TOP) ->
		LOC (h, HAND([], TOP, t))
	| LOC (NODE(h :: t), HAND(left, hand, right)) -> 
		LOC (h, HAND([], HAND(left, hand, right), t))
	| LOC (_, hand) -> raise (NOMOVE "down of top")

