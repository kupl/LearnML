exception NOMOVE of string

type item = string
type tree = LEAF of item | NODE of tree list
type zipper = TOP | HAND of tree list * zipper * tree list
type location = LOC of tree * zipper

let goLeft loc = match loc with
	LOC(NODE [], _) -> raise (NOMOVE "empty tree")
  | LOC(t, TOP) -> raise (NOMOVE "left of top")
  | LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
  | LOC(t, HAND([], up, right)) -> raise (NOMOVE "left of first")

let goRight loc = match loc with
	LOC(NODE [], _) -> raise (NOMOVE "empty tree")
  | LOC(t, TOP) -> raise (NOMOVE "right of top")
  | LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
  | LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of last")

let goUp loc =
	let rec appendtree (left, right) = match left with
		[] -> right
	  | l::remainder -> appendtree (remainder, l::right)
	in
	match loc with
		LOC(NODE [], _) -> raise (NOMOVE "empty tree")
	  | LOC(t, TOP) -> raise (NOMOVE "up of top")
	  | LOC(t, HAND(left, up, right)) -> LOC(NODE(appendtree(left, t::right)), up)

let goDown loc = match loc with
	LOC(LEAF l, _) -> raise (NOMOVE "down of leaf")
  | LOC(NODE [], _) -> raise (NOMOVE "empty tree")
  | LOC(NODE(t1::remainder), zip) -> LOC(t1, HAND([], zip, remainder))