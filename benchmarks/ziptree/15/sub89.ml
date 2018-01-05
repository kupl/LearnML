type item = string
type tree = LEAF of item
					| NODE of tree list

type zipper = TOP
					| HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

exception NOMOVE of string

let goLeft loc = match loc with
	| LOC (t, TOP) -> raise ( NOMOVE "left of top" )
	| LOC (t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
	| LOC (t, HAND([],up,right)) -> raise ( NOMOVE "left of first" )


let goRight loc = match loc with
	| LOC (t, TOP) -> raise ( NOMOVE "right of top" )
	| LOC (t, HAND(left, up, r::right)) -> LOC ( r, HAND( t::left, up, right ))
	| LOC (t, HAND(left,up,[])) -> raise ( NOMOVE "right of first")

	
let goUp loc = match loc with
	| LOC(t, TOP) -> raise ( NOMOVE "up of top")
	| LOC(t, HAND(left, up, right)) -> LOC( NODE (List.rev left@[t]@right), up ) 

let getDownOfLeftMost = function
	| LEAF l -> raise (NOMOVE "down nomove")
	| NODE n -> if n = [] then raise (NOMOVE "down nomove") else (List.hd n, List.tl n)
	
let goDown loc = match loc with
	| LOC(t, TOP) ->  
			let (leftdown, leftover) = getDownOfLeftMost t in
			LOC( leftdown, HAND( [], TOP, leftover ) )

	| LOC(t, HAND(left,up,right)) ->  
			let (leftdown, leftover) = getDownOfLeftMost t in
			LOC( leftdown, HAND( [],  HAND( left, up, right ), leftover) )
(*
let l = LOC( NODE[ NODE[LEAF "a"; LEAF "*"; LEAF "b"] ; LEAF "+"; NODE[ LEAF "c"; LEAF "*"; LEAF "d"] ], TOP )
*)
