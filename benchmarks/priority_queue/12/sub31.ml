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

let rec merge (h1, h2) =
        match (h1,h2) with
        | (EMPTY,h2) -> h2
        | (h1, EMPTY) -> h1
        | (h1,h2) -> if ((findMin h1) <= (findMin h2)) then shake ((findMin h1),(deleteMin h1),h2)
                        else shake ((findMin h2),h1,(deleteMin h2))
and findMin h = match h with
        | EMPTY -> raise EmptyHeap
        | NODE(_,x,_,_) -> x

and insert(x,h) = merge(h, NODE(0,x,EMPTY,EMPTY))

and deleteMin h = match h with
        | EMPTY -> raise EmptyHeap
        | NODE(_,x,lh,rh) -> merge (lh,rh)

