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

let findMin h = match h with 
  | EMPTY -> raise EmptyHeap 
    | NODE(_,x,_,_) -> x 



let rec merge (lh, rh) = 
    match lh with
    | EMPTY -> rh
    | NODE(r,x,llh,rrh) -> 
        let lh_min, rh_min = (findMin lh), (findMin rh) in
        if lh_min > rh_min then merge(rh,lh)
        else
          let h = merge(rrh, rh) in
          shake(x,llh,h)
  

let insert(x,h) = merge(h, NODE(0,x,EMPTY,EMPTY)) 

let deleteMin h = match h with 
  | EMPTY -> raise EmptyHeap 
    | NODE(_,x,lh,rh) -> merge (lh,rh) 

