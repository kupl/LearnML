let rec listconcat li1 li2 =
	match li1 with
		[] -> li2
		| x::y -> x::(listconcat y li2)

type item = string (*이것은 편의상 이렇게 한 것임..*)

type tree 	= LEAF of item
			| NODE of tree list

type zipper = TOP
			| HAND of tree list * zipper * tree list
;;
type location = LOC of tree * zipper

exception NOMOVE of string

let goLeft loc = 
	match loc with
		LOC(t,TOP) -> raise (NOMOVE "left of top")
		| LOC(t, HAND(l::left, up, right)) -> LOC(l , HAND(left, up, t::right))
		| LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first")

let rec listreverse li =
	match li with
		[] -> []
		| x::y -> listconcat (listreverse y) ([x])

let goUp loc =
	match loc with
		LOC(t,TOP) -> raise (NOMOVE "up of TOP")
		| LOC(t,HAND(left,zip,right)) -> 
			let uptree = NODE (listconcat (listreverse left) (t::right)) in
			(match zip with
			 	TOP -> LOC(uptree,TOP)
				| z -> LOC(uptree,z)
			)
;;

let goRight loc = 
	match loc with
		LOC(t,TOP) -> raise (NOMOVE "right of top")
		| LOC(t, HAND(left, up, l::right)) -> LOC(l , HAND(t::left, up, right))
		| LOC(t, HAND(left,up,[])) -> raise (NOMOVE "right of first")
;;

let goDown loc = 
	match loc with
		LOC(LEAF l,z) -> raise (NOMOVE "down of leaf")
		| LOC(NODE [],z) -> raise (NOMOVE "down of null leaf")
		| LOC(NODE (x::y),z) -> LOC(x,HAND([],z,y))
;;
