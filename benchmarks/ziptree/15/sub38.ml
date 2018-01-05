type item = string
type tree = LEAF of item
           | NODE of tree list

type zipper = TOP | HAND of tree list * zipper * tree list
type location = LOC of tree * zipper


exception NOMOVE of string

let goLeft loc = match loc with
                | LOC (t, TOP) -> raise (NOMOVE "left of top")
								| LOC (t, HAND (l::left, up, right)) -> LOC (l, HAND (left, up, t::right))
								| LOC (t, HAND ([], up, right)) -> raise (NOMOVE "left of first")

let goRight loc = 
	match loc with
	| LOC (t, TOP) -> raise (NOMOVE "right of top")
	| LOC (t, HAND (left, top, r::right)) -> LOC (r, HAND (t::left, top, right))
	| LOC (t, HAND (left, top, [])) -> raise (NOMOVE "right of the last")


let goUp loc = 
	match loc with
	| LOC (t, TOP) -> raise (NOMOVE "top of top")
	| LOC (m, HAND ([], zip, b)) -> LOC (NODE (m::b), zip)
	| LOC (m, HAND (a, zip, b)) -> LOC (NODE (List.append a (m::b)), zip)


let goDown loc = 
	match loc with
	| LOC(LEAF x, _) -> raise (NOMOVE "bottom of bottom")
	| LOC(t, TOP) -> 
		(match t with
		| NODE l -> 
			(match l with
			| [] -> raise (NOMOVE "empty node")
			| hd::tl -> LOC(hd, HAND([], TOP, tl)))
		| _ -> raise (NOMOVE " "))
	| LOC(t, HAND([], TOP, [])) ->
		(match t with
		| NODE l ->
			(match l with 
			| [] -> raise (NOMOVE "empty node")
			| hd::tl -> LOC(hd, HAND([], TOP, tl)))
		| _ -> raise (NOMOVE " "))
  | LOC(t, HAND(left, up, right)) ->
		(match t with
		| NODE l ->
			(match l with
			| [] -> raise (NOMOVE "empty node")
			| hd::tl -> LOC(hd, HAND([], HAND(left, up, right), tl)))
		| _ -> raise (NOMOVE " "))
  



