type item = string
type tree = LEAF of item
          | NODE of tree list
type zipper = TOP
            | HAND of tree list * zipper * tree list
type location = LOC of tree * zipper

exception NOMOVE of string

let goLeft: location -> location = fun loc ->
  match loc with
    | LOC(_, TOP) -> raise (NOMOVE "left of top")
    | LOC(_, HAND([], _, _)) -> raise (NOMOVE "left of first")
    | LOC(c, HAND(h::t, up, r)) -> LOC(h, HAND(t, up, c::r))

let goRight: location -> location = fun loc ->
  match loc with
    | LOC(_, TOP) -> raise (NOMOVE "right of top")
    | LOC(_, HAND(_, _, [])) -> raise (NOMOVE "right of last")
    | LOC(c, HAND(l, up, h::t)) -> LOC(h, HAND(c::l, up, t))

let goUp: location -> location = fun loc ->
  match loc with
    | LOC(_, TOP) -> raise (NOMOVE "up of top")
    | LOC(c, HAND(l, up, r)) -> LOC(NODE((List.rev l)@c::r), up)

let goDown: location -> location = fun loc ->
  match loc with
    | LOC(LEAF(_), _) -> raise (NOMOVE "down of leaf")
    | LOC(NODE([]), _) -> raise (NOMOVE "NODE must have child")
    | LOC(NODE(h::t), up) -> LOC(h, HAND([], up, t))
