(*
    PL 2-4
    2008-11609 박성원
*)

type item = string
type tree = LEAF of item
          | NODE of tree list
type zipper = TOP
            | HAND of tree list * zipper * tree list
type location = LOC of tree * zipper
exception NOMOVE of string

let goLeft loc =
  match loc with
  | LOC (t, TOP) -> raise (NOMOVE "left of top")
  | LOC (t, HAND (l :: left, up, right)) -> LOC (l, HAND (left, up, t :: right))
  | LOC (t, HAND ([], up, right)) -> raise (NOMOVE "left of first")

let goRight loc =
  match loc with
  | LOC (t, TOP) -> raise (NOMOVE "right of top")
  | LOC (t, HAND (left, up, r :: right)) -> LOC (r, HAND (t :: left, up, right))
  | LOC (t, HAND (left, up, [])) -> raise (NOMOVE "right of last")

let goUp loc =
  match loc with
  | LOC (t, TOP) ->
      raise (NOMOVE "up of top")
  | LOC (t, HAND (left, upzip, right)) ->
      LOC (NODE (left @ [t] @ right), upzip)

let goDown loc =
  match loc with
  | LOC (LEAF item, zip) ->
      raise (NOMOVE "down of leaf")
  | LOC (NODE [], zip) ->
      raise (NOMOVE "no children")
  | LOC (NODE (l :: remain), zip) ->
      LOC (l, HAND ([], zip, remain))
