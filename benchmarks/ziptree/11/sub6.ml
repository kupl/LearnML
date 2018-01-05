type item = string

type tree = LEAF of item | NODE of tree list

type zipper = TOP | HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

exception NOMOVE of string


let goLeft loc =
	
	match loc with
	|LOC(t,TOP) -> raise (NOMOVE "left of top")
	|LOC(t,HAND(l::left,up,right)) -> LOC(l, HAND(left, up, t::right))
	|LOC(t,HAND([],up,right)) -> raise (NOMOVE "left of first")


let goRight loc =
	match loc with
	|LOC(t,TOP) -> raise (NOMOVE "right of top")
	|LOC(t,HAND(left,up,r::right)) -> LOC(r, HAND(t::left, up, right))
	|LOC(t,HAND(left,up,[])) -> raise (NOMOVE "right of first")

let goUp loc =
	match loc with
	|LOC(t,TOP) -> raise (NOMOVE "up of top")
	|LOC(t,HAND(left,up,right)) -> 
		match up with
		|TOP -> LOC(NODE(List.rev(left)@(t::List.rev(right))), TOP)
		|HAND(upleft, upup, upright) -> LOC(NODE(List.rev(left)@(t::List.rev(right))),HAND(upleft,upup,upright)) 

let goDown loc =
	let mtch (a,b) =
		match a with
		|LEAF(team') -> raise (NOMOVE "BOTTOM")	
		|NODE([]) -> raise(NOMOVE "INPUT : NODE []")
		|NODE(lst1::lst) -> LOC(lst1,HAND([],b,lst))	
	in

	match loc with
	|LOC(t,HAND(left,up,right)) -> mtch(t,HAND(left,up,right))
	|LOC(t,TOP) ->
		match t with
		|LEAF(team') -> raise (NOMOVE "BOTTOM")
		|NODE([]) -> raise (NOMOVE "INPUT : NODE[]")
		|NODE(lst1::lst) -> LOC(lst1,HAND([],TOP,lst))

