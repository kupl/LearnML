(* 2009-11824 Jieun-Jeong HW2-4 *)

type heap = EMPTY | NODE of rank * value * heap * heap 
and rank = int 
and value = int 

exception EmptyHeap 

let rank h = match h with 
	| EMPTY -> -1 
	| NODE(r,_,_,_) -> r 

let shake (x,lh,rh) = 
	if (rank lh) >= (rank rh) 
		then NODE(rank rh+1, x, lh, rh) 
		else NODE(rank lh+1, x, rh, lh) 

let rec merge hPair =
	let getValue h = match h with
					|EMPTY			-> raise EmptyHeap
					|NODE(_,x,_,_)	-> x in
	let getLeft h = match h with
					|EMPTY			-> raise EmptyHeap
					|NODE(_,_,lh,_)	-> lh in
	let getRight h	= match h with
					|EMPTY			-> raise EmptyHeap
					|NODE(_,_,_,rh)	-> rh in
	match hPair with
	|(EMPTY, h2)	-> h2
	|(h1, EMPTY)	-> h1
	|(h1, h2)		->
		if (getValue h1) > (getValue h2)
			then merge (h2, h1)	(* h1�� root�� �� ����̹Ƿ� x1 <= x2 ���� �� *)
			else shake ((getValue h1), (getLeft h1), (merge ((getRight h1), h2)))
			(*let new_rchild = merge ((getRight h1), h2) in (* balance�� ���߱� ���� �ϴ�  h1�� rchild�� h2�� merge�Ѵ� *)
				if (rank (getLeft h1)) < (rank new_rchild)
					then NODE(((getRank (getLeft h1)) + 1), (getValue h1), new_rchild, (getLeft h1)) (* rchild�� rank�� Ŭ ��� lchild�� rchild�� �ٲ۴�. rank�� lchild�� �� + 1 *)
					else NODE(((getRank new_rchild) + 1), (getValue h1), (getLeft h1), new_rchild) *)

let findMin h = match h with 
	| EMPTY -> raise EmptyHeap 
    | NODE(_,x,_,_) -> x 

let insert(x,h) = merge(h, NODE(0,x,EMPTY,EMPTY)) 

let deleteMin h = match h with 
	| EMPTY -> raise EmptyHeap 
	| NODE(_,x,lh,rh) -> merge (lh,rh) 
