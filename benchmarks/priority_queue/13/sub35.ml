(* 4190.310 Programming Languages - Daegeun Lee <elnn@elnn.kr> *)

type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap

let rank = fun heap ->
    match heap with
    | EMPTY -> -1
    | NODE(r,_,_,_) -> r

let findMin = fun heap ->
    match heap with
    | EMPTY -> raise EmptyHeap
    | NODE(_,x,_,_) -> x


let rec merge = function (lh, rh) ->
    
    let left = fun heap ->
        match heap with
        | EMPTY -> raise EmptyHeap
        | NODE(_,_,lh,_) -> lh
    in
    
    let right = fun heap ->
        match heap with
        | EMPTY -> raise EmptyHeap
        | NODE(_,_,_,rh) -> rh
    in

    let shake = fun (x,lh,rh) ->
        if (rank lh) >= (rank rh)  (* leftist heap *)
        then NODE(rank rh + 1, x, lh, rh)
        else NODE(rank lh + 1, x, rh, lh)
    in
    
    match (lh, rh) with
    | EMPTY, rh -> rh
    | lh, EMPTY -> lh
    | lh, rh ->
        let (small, big) = if (findMin lh) < (findMin rh) then (lh, rh) else (rh, lh) in
            shake (findMin small, left small, merge(right small, big))


let deleteMin = fun heap ->
    match heap with
    | EMPTY -> raise EmptyHeap
    | NODE(_,_,lh,rh) -> merge(lh, rh)

let insert = fun (e, heap) ->
    merge(heap, NODE(0, e, EMPTY, EMPTY))

