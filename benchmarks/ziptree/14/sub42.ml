exception NOMOVE of string
exception Todo

type item = string
type tree = LEAF of item
          | NODE of tree list
type zipper = TOP
            | HAND of tree list * zipper * tree list
type location = LOC of tree * zipper

let goLeft loc =
  match loc with
  | LOC (t, TOP) -> raise (NOMOVE "left of top")
  | LOC (t, HAND (l::left, up, right)) -> LOC (l, HAND(left, up, t::right))
  | LOC (t, HAND ([], up, right)) -> raise (NOMOVE "left of first")

let goRight (loc: location): location =
  match loc with
  | LOC (t, TOP) -> raise (NOMOVE "right of top")
  | LOC (t, HAND (left, up, r::right)) -> LOC (r, HAND (t::left, up, right))
  | LOC (t, HAND (left, up, [])) -> raise (NOMOVE "right of last")

let goUp (loc: location): location =
  match loc with
  | LOC (t, TOP) -> raise (NOMOVE "up of top")
  | LOC (t, HAND (left, up, right)) -> let new_tree =
                                         List.append (List.rev left) (t::right)
                                       in
                                       LOC (NODE new_tree, up)

(* Move to child's left node *)
let rec goDown (loc: location): location =
  match loc with
  | LOC (LEAF l, _) -> raise (NOMOVE "down of bottom")
  | LOC (NODE tree, hand) -> LOC (List.hd tree, HAND ([], hand, List.tl tree))
