(*컴퓨터공학부/2011-11729/안진우/2-5*)

exception NOMOVE of string
type item = string
type tree = LEAF of item
          | NODE of tree list

type zipper = TOP
            | HAND of tree list * zipper * tree list          

type location = LOC of tree * zipper

let goLeft (loc : location) : location  =
        match loc with
        | LOC(t, TOP) -> raise (NOMOVE "left of top")
        | LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
        | LOC(t, HAND([], up, right)) -> raise (NOMOVE "left of first")

let goRight (loc : location) : location =
        match loc with
        | LOC(t, TOP) -> raise (NOMOVE "left of top")
        | LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
        | LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of last")

let goDown (loc : location) : location =
        match loc with
        | LOC(NODE[], _) -> raise (NOMOVE "down of empty node") 
        | LOC(LEAF _, _) -> raise(NOMOVE "down of bottom") 
        | LOC(NODE(first::sibling), zipper) -> LOC(first, HAND([], zipper, sibling))

let goUp (loc : location) : location =
        match loc with
        | LOC(_, TOP) -> raise (NOMOVE "up of top")
        | LOC(t, HAND(left, up, right)) -> LOC(NODE(List.append (List.append (List.rev left) (t::[])) right), up) 