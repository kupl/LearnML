type item = string
type tree =
  | LEAF of item
  | NODE of tree list
type zipper =
  | TOP
  | HAND of tree list * zipper * tree list
type location = LOC of tree * zipper
exception NOMOVE of string


(*let rec print_t t =
  let print_l =
    fun a b -> if a = "" then print_t b else a ^ ", " ^ (print_t b) in
  match t with
  | LEAF i -> i
  | NODE l -> "[" ^ List.fold_left print_l "" l ^ "]"
let print_l =
  fun a b -> if a = "" then print_t b else a ^ ", " ^ (print_t b)
let rec print_z z =
  match z with
  | TOP -> "TOP"
  | HAND (l1, z', l2) ->
    let print_tl l = (List.fold_left print_l "" l) in
    "l: {" ^ (print_tl l1) ^ "}, z: {" ^ (print_z z') ^ "}, r: {" ^ (print_tl l2) ^ "}"
let print_loc l =
  match l with
  LOC (tree, zipper) ->
    "current: <" ^ (print_t tree) ^ ">, zip: <" ^ (print_z zipper) ^ ">"
let loc =
  LOC (LEAF "*",
  HAND([LEAF "c"],
  HAND([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]],
  TOP,
  []),
  [LEAF "d"]))
*)


let goLeft (loc : location) : location =
  match loc with
  | LOC(t, TOP) -> raise (NOMOVE "left of top")
  | LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
  | LOC(t, HAND([], up, right)) -> raise (NOMOVE "left of first")

let goRight (loc : location) : location =
  match loc with
  | LOC(t, TOP) -> raise (NOMOVE "right of top")
  | LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
  | LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of last")

let goUp (loc : location) : location =
  match loc with
  | LOC(t, TOP) -> raise (NOMOVE "up of top")
  | LOC(t, HAND(left, up, right)) -> LOC(NODE ((List.rev left) @ (t :: right)), up)

let goDown (loc : location) : location =
  match loc with
  | LOC(LEAF _, _) -> raise (NOMOVE "down of leaf")
  | LOC(NODE(hd::tl), zipper) -> LOC(hd, HAND([], zipper, tl))
  | LOC(NODE [], zipper) -> raise (NOMOVE "invalid tree")

(*let () = print_endline(print_loc loc)
let () = print_endline(print_loc(goLeft loc))
let () = print_endline(print_loc(goRight loc))
let () = print_endline(print_loc(goUp loc))
let () = print_endline(print_loc(goDown(goUp loc)))
*)

