exception NOMOVE of string

type item = string
and tree = LEAF of item
	   | NODE of tree list
and zipper = TOP
	     | HAND of tree list * zipper * tree list
and location = LOC of tree * zipper

let goLeft loc = 
  match loc with
  | LOC(t, TOP) -> raise (NOMOVE "left of top")
  | LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
  | LOC(t, HAND([], up, right)) -> raise (NOMOVE "left of first") 

let goRight (loc: location): location =
  match loc with
  | LOC(t, TOP) -> raise (NOMOVE "right of top")
  | LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
  | _ -> raise (NOMOVE "goRight_NOMOVE") 

let goUp (loc: location): location = 
  match loc with
  | LOC(t, TOP) -> raise (NOMOVE "up of top")
  | LOC(t, HAND(left, up, right)) -> LOC(NODE((List.rev left)@(t::right)), up)

let goDown (loc: location): location = 
  match loc with
  | LOC(LEAF a, _) -> raise (NOMOVE "Cannot go down from a leaf")
  | LOC(NODE(t1::trees), ctx) -> LOC(t1, HAND([], ctx, trees))
  | _ -> raise (NOMOVE "goDown_NOMOVE")


