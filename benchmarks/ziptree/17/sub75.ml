exception NOMOVE of string
type item = string
type tree = LEAF of item | NODE of tree list

type zipper = TOP | HAND of tree list * zipper * tree list
type location = LOC of tree * zipper

let goLeft loc = match loc with
	 LOC(t,TOP) -> raise (NOMOVE "left of top")
	|LOC(t,HAND(l::left, up, right)) -> LOC(l, HAND(left,up,t::right))
	|LOC(t,HAND([],up,right)) -> raise (NOMOVE "left of first")

let goRight : location -> location = fun(currloc)
->  match currloc with
	 LOC(t,TOP) -> raise (NOMOVE "right of top")
	|LOC(t,HAND(left,up,r::right)) -> LOC(r,HAND(t::left,up,right))
	|LOC(t,HAND(left,up,[])) -> raise (NOMOVE "right of last")

let rec append: 'a list * 'a list -> 'a list = fun(list1,list2) ->
        match list1 with
                 [] -> list2
                |hd::tail -> hd::append(tail,list2)

let rec reverse_list: 'a list -> 'a list = fun(list_)
-> match list_ with
         [] -> []
        |hd::tail -> append(reverse_list(tail),[hd])


let goUp: location -> location = fun(currloc)
-> match currloc with
	 LOC(t,TOP) -> raise (NOMOVE "can not going up furter.")
	|LOC(t,HAND(left,up_zipper,right)) -> LOC( NODE(append(reverse_list(left),t::right)) , up_zipper)

let goDown: location -> location = fun(currloc)
-> match currloc with
	LOC(t,my_zipper) ->
		(
		  match t with
		  	 LEAF(item) -> raise (NOMOVE "Can not going down further.")
			|NODE(children_list) ->
				 (
				   match children_list with (hd::tail) ->
					 LOC(hd, HAND([],my_zipper,tail))
					|[] -> raise (NOMOVE "Can not going down further.")
				 )
		)

