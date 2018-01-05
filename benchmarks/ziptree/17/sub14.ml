type item = string

type tree = LEAF of item | NODE of tree list

type zipper = TOP | HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

exception NOMOVE of string

let rec addright (lst, a) = match lst with
  |[] -> a::[]
  |b::lst2 -> b::(addright(lst2, a))
let rec reverse lst = match lst with
  |[] -> []
  |hd::tl -> addright(reverse(tl), hd)
let rec mergelist (l1, l2) = match l2 with
  |[] -> l1
  |a::l22 -> mergelist(addright(l1,a),l22)
 
let goLeft loc =  match loc with
  |LOC(t, TOP) -> raise (NOMOVE "left of top")
  |LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
  |LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first")

let goRight loc = match loc with
  |LOC(t, TOP) -> raise (NOMOVE "right of top")
  |LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
  |LOC(t, HAND(left,up,[])) -> raise (NOMOVE "right of first")

let goUp loc = match loc with
  |LOC(t, TOP) -> raise(NOMOVE "up of top")
  |LOC(t, HAND([], up, right)) ->
    LOC(NODE(t::right), up)
  |LOC(t, HAND(left, up, right)) ->
    LOC(NODE(mergelist(reverse(left),t::right)), up)

let goDown loc = match loc with
  |LOC(LEAF _, _) -> raise(NOMOVE "down of leaf")
  |LOC(NODE [], up) -> raise(NOMOVE "down of empty")
  |LOC(NODE(t::tl), up) -> LOC(t, HAND([], up, tl))  

