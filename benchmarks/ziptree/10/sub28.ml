(*hw1-8 컴퓨터 공학부 2008-11641 신희식*) 

exception NOMOVE of string
type item = string
type tree = LEAF of item
			| NODE of tree list
type zipper = TOP
			| HAND of tree list * zipper * tree list
type location = LOC of tree * zipper


let goLeft loc =
	match loc with
	(LOC (t, TOP)) -> 
		(raise (NOMOVE "left of top"))
	|(LOC (t, HAND ([], up, right))) ->
		(raise (NOMOVE "left of first"))
	|(LOC (t, HAND (l::left, up, right))) ->
		(LOC (l, HAND (left,up,t::right)))

let goRight loc =
	match loc with
	(LOC (t, TOP)) ->
		(raise (NOMOVE "right of top"))
	|(LOC (t, HAND (left, up, []))) ->
		(raise (NOMOVE "right of last"))
	|(LOC (t, HAND (left, up, r::right))) ->
		(LOC (r, HAND (t::left, up, right)))

let goUp loc =
	match loc with
	(LOC (t, TOP)) ->
		(raise (NOMOVE "up of top"))
	|(LOC (t, HAND (left, up, right))) ->
		(LOC ((NODE (List.append (List.rev left) (t::right))),
			  up))

let goDown loc =
	match loc with
	(LOC (t, zip)) ->
		(match t with
		 (LEAF a) ->
		 	(raise (NOMOVE "down of leaf"))
		|(NODE []) ->
			(raise (NOMOVE "down of node []"))
		|(NODE (h::t)) ->
			(LOC (h, HAND ([], zip, t)))
		)

