(* 200511843 LEE JONGHO *)


type heap = EMPTY | NODE of rank * value * heap * heap
	and rank = int
	and value = int

exception EmptyHeap

fun rank EMPTY = 0
	| rank NODE(r, , , ) = r
fun insert(x,h) = merge(h, NODE(0,x,EMPTY,EMPTY))
fun findMin EMPTY = raise EmptyHeap
	| findMin NODE( ,x, , ) = x
fun deleteMin EMPTY = raise EmptyHeap
	| deleteMin NODE( ,x,lh,rh) = merge(lh,rh)

let merge (h1, h2) =
	match 

fun shake (x,lh,rh) = if (rank lh) >= (rank rh)
	then NODE(rank rh + 1, x, lh, rh)
	else NODE(rank lh + 1, x, rh, lh)