(* Homework 2 - Exercise 5
 * 2011-10492 Jaeyeong Yang *)
type item = string
type tree = LEAF of item
          | NODE of tree list

type zipper = TOP
            | HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

exception NOMOVE of string

let goLeft: location -> location = fun loc ->
  match loc with
  | LOC(t, TOP) -> raise (NOMOVE "left of top")
  | LOC(t, HAND([], _, _)) -> raise (NOMOVE "left of first")
  | LOC(t, HAND(l :: left, up, right)) -> LOC(l, HAND(left, up, t :: right))

let goRight: location -> location = fun loc ->
  match loc with
  | LOC(t, TOP) -> raise (NOMOVE "right of top")
  | LOC(t, HAND(_, _, [])) -> raise (NOMOVE "right of first")
  | LOC(t, HAND(left, up, l :: right)) -> LOC(l, HAND(t :: left, up, right))

let goUp: location -> location = fun loc ->
  match loc with
  | LOC(t, TOP) -> raise (NOMOVE "Cannot zip up the TOP")
  | LOC(t, HAND(l, z, r)) ->
    let tt = NODE (List.rev_append l (t :: r)) in
    LOC(tt, z)

let goDown: location -> location = fun loc ->
  match loc with
  | LOC(t, z) ->
    (match t with
     | NODE (th :: tt) -> LOC (th, HAND([], z, tt))
     | _ -> raise (NOMOVE "down of bottom"))
