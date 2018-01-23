exception Error of string
type item = string
exception NOMOVE of string
type tree = LEAF of item
  | NODE of tree list
type zipper = TOP
  | HAND of tree list * zipper * tree list
type location = LOC of tree * zipper

let goDown : location -> location =
  fun location ->
      (
        match location
        with LOC(LEAF(i), u) -> raise (NOMOVE("down of buttom"))
          | LOC(NODE([]), u) -> raise (NOMOVE("down of first"))
          | LOC(NODE(t::ts), u) -> LOC(t,HAND([],u,ts))
      ) 
let goUp : location -> location =
  fun location ->
      (
        match location
        with LOC(t, TOP) -> raise (NOMOVE("up of top"))
          | LOC(t,HAND(l, TOP, r)) -> raise (NOMOVE("up of first"))
          | LOC(t,HAND(l, u,r)) -> LOC(NODE(l@(t::r)), u)
      ) 
let goLeft : location -> location =
  fun location ->
      (
        match location
        with LOC(t, TOP) -> raise (NOMOVE "left of top")
          | LOC(t,HAND([], u, r)) -> raise (NOMOVE "left of first")
          | LOC(t,HAND(l::ls,u,r)) -> LOC(l,HAND(ls,u,t::r))
      ) 

let goRight : location -> location =
  fun location ->
      (
        match location
        with LOC(t, TOP) -> raise (NOMOVE("right of top"))
          | LOC(t,HAND(l, u, [])) -> raise (NOMOVE("right of first"))
          | LOC(t,HAND(l, u, r::rs)) -> LOC(r,HAND(t::l,u,rs))
      )


