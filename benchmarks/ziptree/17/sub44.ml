type item = string
type tree = LEAF of item
          | NODE of tree list
type zipper = TOP
            | HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

exception NOMOVE of string

let goLeft loc = match loc with
  | LOC(t, TOP) -> raise (NOMOVE "left of top")
  | LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
  | LOC(t, HAND([], up, right)) -> raise (NOMOVE "left of first")

let goRight loc = match loc with
  | LOC(t, TOP) -> raise (NOMOVE "right of top")
  | LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
  | LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of last")

let goUp loc = match loc with
  | LOC(t, TOP) -> raise (NOMOVE "up of top")
  | LOC(t, HAND(l::left, up, r::right)) -> LOC(NODE [l; t; r], up)
  | LOC(t, HAND([], up, l::r::right)) -> LOC(NODE[t; l; r], up)
  | LOC(t, HAND(l::r::rith, up, [])) -> LOC(NODE[r; l; t], up)
  | LOC(t, HAND([], up, [])) -> LOC(t, up)
  | _ -> raise (NOMOVE "wrong tree")

let goDown loc = match loc with
  | LOC(LEAF a, hand) -> raise (NOMOVE "down of botton")
  | LOC(NODE [a; b; c], hand) -> LOC(a, HAND([], hand, [b; c]))
  | _ -> raise (NOMOVE "wrong tree")

let rec printTree tree = match tree with
  | LEAF a -> print_string ("LEAF "^a^"; ")
  | NODE [a; b; c] -> print_string "NODE ["; printTree a;
                     printTree b; printTree c; print_string "] "
  | _ -> raise (NOMOVE "wrong tree")

let rec printTreeList li = match li with
  | [] -> print_string "]"
  | h::t -> printTree h; printTreeList t

let rec printHand hand = match hand with
  | TOP -> print_string "TOP"
  | HAND(left, up, right) -> print_string "["; printTreeList left; 
                             printHand up; print_string "["; printTreeList right
let printLocation loc = match loc with
  | LOC(t, hand) -> print_string "LOC ("; printTree t; 
                    print_string ", "; print_string "HAND (";
                    printHand hand; print_endline "))"


let tree = NODE [ NODE [LEAF "a"; LEAF "*"; LEAF "b"]; LEAF "+"; 
           NODE [LEAF "c"; LEAF "*"; LEAF "d"]]

