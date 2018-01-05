type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap

let rank = function EMPTY -> -1
                |NODE (r,_,_,_) -> r
        
let merge (a,b) =
    let shake = function (x,lh,rh) -> 
        if (rank lh) >= (rank rh)
            then NODE(rank rh +1, x, lh, rh)
            else NODE(rank lh +1, x, rh, lh) in
    let rec submerge (c,d) =
        match (c,d) with
            |(EMPTY,EMPTY) -> EMPTY
            |(EMPTY,k1) -> k1
            |(k2,EMPTY) -> k2
            |(NODE(r1,v1,lh1,rh1),NODE(r2,v2,lh2,rh2))
                -> if v1 > v2 then submerge(NODE(r2,v2,lh2,rh2),NODE(r1,v1,lh1,rh1))
                    else if rh1 = EMPTY then shake (v1,lh1,NODE(r2,v2,lh2,rh2))
                        else shake (v1,lh1,submerge(rh1,NODE(r2,v2,lh2,rh2)))
    in submerge (a,b)

let insert = function (x,h) -> merge(h, NODE(0,x,EMPTY,EMPTY))

let findMin = function EMPTY -> raise EmptyHeap
                    |NODE(_,x,_,_) -> x

let deleteMin = function EMPTY -> raise EmptyHeap
                    |NODE(_,x,lh,rh) -> merge(lh,rh)
