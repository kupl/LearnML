(* HW2 Exercise 5 zip-zip tree *)
exception NOMOVE of string

type item = string
type tree = LEAF of item
          | NODE of tree list

type zipper = TOP
            | HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

let goLeft : location -> location = fun loc ->
  match loc with
  | LOC(t, TOP) -> raise (NOMOVE "left of top")
  | LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
  | LOC(t, HAND([], up, right)) -> raise (NOMOVE "left of first")

let goRight : location -> location = fun loc ->
  match loc with
  | LOC(t, TOP) -> raise (NOMOVE "right of top")
  | LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
  | LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of last")

let goUp : location -> location = fun loc ->
  match loc with
  | LOC(t, TOP) -> raise (NOMOVE "top of top")
  | LOC(t, HAND(left, up, right)) -> (
      let rec append: tree list * tree list -> tree list = fun (list1, list2) ->
        match list1 with
        | [] -> list2
        | head::tail -> append (tail, head::list2)
      in
      LOC(NODE(append (left, t::right)), up)
    )
let goDown : location -> location = fun loc ->
  match loc with
  | LOC(LEAF l, zip) -> raise (NOMOVE "down of leaf")
  | LOC(NODE [], zip) -> raise (NOMOVE "down of leaf")
  | LOC(NODE (head::tail), zip) -> LOC(head, HAND([], zip, tail))
