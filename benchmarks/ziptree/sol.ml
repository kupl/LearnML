(*ziptree*)
(*http://ropas.snu.ac.kr/~kwang/4190.310/09/hw2.pdf 4번*)
exception NOMOVE of string

type item = string

type tree = LEAF of item
	| NODE of tree list

type zipper = TOP
	| HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

let rec rev_append l1 l2  =
  match l1 with
  |[] -> l2
  |hd::tl -> rev_append tl (hd::l2)

let goLeft loc = match loc with
	| LOC(t, TOP) -> raise (NOMOVE "left of top")
	| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
	| LOC(t, HAND([], up, right)) -> raise (NOMOVE "left of first")

let goRight loc = match loc with
	| LOC(_, TOP) -> raise (NOMOVE "right of top")
	| LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
	| LOC(_, HAND(_, _, [])) -> raise (NOMOVE "right of last")

let goUp loc = match loc with
	| LOC(_, TOP) -> raise (NOMOVE "up of top")
	| LOC(t, HAND(left, up, right)) -> LOC(NODE(rev_append left (t::right)), up)

let goDown loc = match loc with
	| LOC(LEAF _, _) -> raise (NOMOVE "down of leaf")
	| LOC(NODE [], _) -> raise (NOMOVE "down of empty node")
	| LOC(NODE (h::t), zip) -> LOC(h, HAND([], zip, t))
