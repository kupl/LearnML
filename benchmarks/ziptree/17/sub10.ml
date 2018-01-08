type item = string
type tree = LEAF of item
| NODE of tree list

type zipper = TOP
| HAND of tree list * zipper * tree list

exception NOMOVE of string

type location = LOC of tree * zipper



let goLeft loc = 
  match loc with
  |LOC(t, TOP) -> raise (NOMOVE "left of top")
  |LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
  |LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first")


let goRight loc = 
  match loc with
  |LOC(t, TOP) -> raise (NOMOVE "right of top")
  |LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
  |LOC(t, HAND(left,up,[])) -> raise (NOMOVE "right of first")


let goUp loc = 

  let reverseList (l:tree list) =
    let rec add now left =
      match left with
      |[] -> now
      |h::t -> add (h::now) t
    in
    add [] l
  in

  match loc with
  |LOC(t, TOP) -> raise (NOMOVE "up of top")
  |LOC(t, HAND(left, up, right)) -> LOC(NODE ((reverseList left)@[t]@right), up)  



let goDown loc = 
  match loc with  
  |LOC(LEAF(item), zipper) -> raise (NOMOVE "down of leaf")
  |LOC(NODE([]), zipper) -> raise (NOMOVE "no child node")
  |LOC(NODE(h::t),zipper) -> LOC(h,HAND([],  zipper ,t) )


  