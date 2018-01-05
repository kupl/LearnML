type item = string
type tree = LEAF of item
            | NODE of tree list
type zipper = TOP
            | HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

exception NOMOVE of string

let goLeft loc = 
  match loc with
  |LOC(t, TOP) -> raise (NOMOVE "left of top")
  |LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
  |LOC(t, HAND([], up, right)) -> raise (NOMOVE "left of first")

let goRight loc = 
  match loc with
  |LOC(t, TOP) -> raise (NOMOVE "left of top")
  |LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
  |LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of last")

let goUp loc = 
  match loc with
  |LOC(t, TOP) -> raise (NOMOVE "up of top")
  |LOC(t, HAND(left_tree_list, up_zipper, right_tree_list)) -> 
    let new_tree = NODE(List.rev_append left_tree_list (t::right_tree_list)) in
    LOC(new_tree, up_zipper)

let goDown loc = 
  match loc with
  |LOC(NODE tree_list, zipper) ->
    if List.length tree_list = 0 then
      raise (NOMOVE "down of empty NODE tree")
    else
      LOC(List.hd tree_list, HAND([], zipper, List.tl tree_list))
  |LOC(LEAF _ , zipper) -> raise (NOMOVE "down of LEAF tree")

(*
let loc = 
    LOC (LEAF "*",
        HAND(
            [LEAF "c"],
            HAND(
                [LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]],
                TOP,
                []),
            [LEAF "d"]))


let loc1 = goLeft loc 

let loc2 = goUp loc

let loc3 = goUp (goUp loc)

let ans1 =
    LOC (LEAF "c", 
        HAND([], 
            HAND(
                [LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]], 
                TOP, 
                []), 
            [LEAF "*"; LEAF "d"])) 


let ans2 = 
    LOC (NODE[LEAF "c"; LEAF "*"; LEAF "d"], 
        HAND(
            [LEAF "+"; NODE[LEAF "a"; LEAF "*"; LEAF "b"]],
            TOP,
            []) 
        )

let ans3 = 
    LOC (NODE [NODE [LEAF "a"; LEAF "*"; LEAF "b"]; 
                LEAF "+"; 
                NODE [LEAF "c"; LEAF "*"; LEAF "d"]]
        , TOP) 



let _ = print_endline (string_of_bool (loc1 = ans1))
let _ = print_endline (string_of_bool (loc2 = ans2))
let _ = print_endline (string_of_bool (loc3 = ans3))
*)
