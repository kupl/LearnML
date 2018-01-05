exception NOMOVE of string
type item = string
type tree = LEAF of item
	|NODE of tree list
type zipper = TOP
	|HAND of tree list*zipper*tree list
type location = LOC of tree*zipper


let rec goRight e =
   match e with 
|LOC(t,TOP) -> raise (NOMOVE "right of Top")
|LOC(t, HAND(left, up, r::right)) -> LOC(r,HAND(t::left,up,right))
|LOC(t,HAND(left,up,[])) ->  raise (NOMOVE "right of last")

let rec goUp f =
   match f with 
    |LOC(t,TOP) -> raise (NOMOVE "up of Top")
    |LOC(t,HAND(left,up,right)) -> LOC(NODE(List.rev left@t::right),up)
    
let rec goDown g =
   match g with  
   |LOC(LEAF t,b) -> raise (NOMOVE "down of LEAF")
   |LOC(NODE(h1::t1), b) -> LOC(h1, HAND([], b, t1))
   |LOC(NODE [],b)-> raise (NOMOVE "down of LEAF")
