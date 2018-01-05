(* 컴퓨터공학부 / 2005-11721 / 김재경 / 숙제1-8 *)
type tree = LEAF of item | NODE of tree list
and item = string
type zipper = TOP
            | HAND of tree list * zipper * tree list
type location = LOC of tree * zipper;;
exception NOMOVE of string
let goRight loc = match loc with
    LOC(t, TOP) -> raise(NOMOVE "right of top")
  | LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
  | LOC(t, HAND(left, up, [])) -> raise(NOMOVE "right of first")
let goUp loc = match loc with
    LOC(t,TOP) -> raise(NOMOVE "alredy top")
  | LOC(t,HAND(left, up, right)) -> LOC(NODE(List.rev(left) @ (t::right)), up)
let goDown loc = match loc with
    LOC(LEAF a, _) -> raise(NOMOVE "LEAF error")
  | LOC(NODE [], _) -> raise(NOMOVE "null NODE error")
  | LOC(NODE(a::right), z) -> LOC(a, HAND([], z, right))