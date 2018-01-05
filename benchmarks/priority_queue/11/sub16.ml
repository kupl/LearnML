exception EmptyHeap

type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

let rank h = match h with
| EMPTY -> -1
| NODE (r, _, _, _) -> r

let shake (x,lh,rh) = 
    if (rank lh) >= (rank rh)
    then NODE(rank rh + 1, x, lh, rh)
    else NODE (rank lh + 1, x, rh, lh)

let rec merge(lh, rh) = 
    match lh with
    | EMPTY -> rh
    | NODE (lrank, lvalue, lha, lhb) -> (match rh with
    | EMPTY -> lh
    | NODE (rrank, rvalue, rha, rhb) -> if lvalue < rvalue then
        shake(lvalue, lha, merge(lhb,rh))
    else shake(rvalue, rha, merge(lh,rhb))
    )

let findMin h = match h with
    | EMPTY -> raise EmptyHeap
    | NODE(_, x, _, _) -> x


let insert(x,h) = merge(h, NODE(0,x,EMPTY,EMPTY))

let deleteMin h = match h with
    | EMPTY -> raise EmptyHeap
    | NODE(_,x,lh,rh) -> merge(lh,rh)



