exception NOMOVE of string
type item = string

type tree = LEAF of item
		  | NODE of tree list

type zipper = TOP
			| HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

let goLeft loc = match loc with
					  LOC(t, TOP) -> raise (NOMOVE "left of top")
					| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
					| LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first")


let goRight loc = match loc with
					   LOC(t, TOP) -> raise (NOMOVE "right of top")
					 | LOC(t, HAND(left,up,r::right)) -> LOC(r, HAND(t::left,up,right))
					 | LOC(t, HAND(left,up,[])) -> raise (NOMOVE "right of first")
let goUp loc = match loc with
					LOC(t, TOP) -> raise (NOMOVE "top")
				  | LOC(t, HAND(left,up,right)) -> LOC(NODE((List.rev (t::left))@right),up)
				  									
let rec goDown loc = match loc with
					  LOC(t, TOP) -> (match t with
					  					  LEAF item -> raise (NOMOVE "bottom")
										| NODE treelist -> (match treelist with
																  [] -> raise (NOMOVE "bottom")
																| l::tree -> LOC (l, HAND([],TOP,tree))))
					| LOC(t, HAND([],up,right)) -> loc
					| LOC(t, HAND(left,up,right)) -> goDown (goLeft loc)
