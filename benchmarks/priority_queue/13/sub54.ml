(*
	department : computer science & engineering
	student ID : 2012-11242 / name : Seon-bi, Park
*)

type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap
let rank = function EMPTY -> -1
					| NODE(r,_,_,_) -> r

let shake = function (x,lh,rh) ->
    if (rank lh) >= (rank rh)
        then NODE(rank rh + 1, x, lh, rh)
    else NODE(rank lh + 1, x, rh, lh)

let findMin = function EMPTY -> raise EmptyHeap
                        | NODE(_,x,_,_) -> x

let rec premerge (lh, rh) =                     (* heap * heap -> heap *)
    match (lh, rh) with
	| (EMPTY, EMPTY) -> EMPTY
	| (EMPTY, NODE(_,_,_,_)) -> rh
	| (NODE(_,_,_,_), EMPTY) -> lh
	| (NODE(_,_,lhltree,lhrtree), (NODE(_,_,rhltree,rhrtree))) ->
		if ((findMin lh) < (findMin rh)) then NODE(0, (findMin lh), lhltree, premerge(lhrtree, rh))
		else NODE(0, (findMin rh), rhltree, premerge(lh, rhrtree))

let arrange htree =                             (* heap -> heap *)
	match htree with
		| EMPTY -> EMPTY
		| NODE(r, x, lh, rh) -> shake(x, lh, rh)

let merge (lh, rh) = arrange( premerge(lh, rh))	(* heap * heap -> heap *)

let insert = function (x,h) -> merge(h, NODE(0,x,EMPTY,EMPTY))

let deleteMin = function EMPTY -> raise EmptyHeap
						| NODE(_,x,lh,rh) -> merge(lh,rh)
