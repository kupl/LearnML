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

let rec merge(heap1, heap2) =
  match (heap1, heap2) with 
  | (_, EMPTY) -> heap1
  | (EMPTY, _) -> heap2
  | (NODE(lr, lv, lh1, lh2), NODE(rr, rv, rh1, rh2)) -> (
    (* assume : value of h1 <= value of h2 *)
    if lv > rv then
      merge(heap2, heap1)
    else 
      begin
        let heap = shake(lv, lh1, merge(lh2, heap2)) in
        (* ��ģ �� ������ Ʈ���� rank�� �� Ŭ��� ���ʰ� ������Ŵ. �׻� ������
         * �� �浵��  *)
        match heap with
        | EMPTY -> heap
        | NODE(r, v, lh, rh) -> (
          if (rank lh) < (rank rh) then
            NODE(r, v, rh, lh)
          else
            heap
        )
      end
  )
  

let findMin h = match h with 
| EMPTY -> raise EmptyHeap 
| NODE(_,x,_,_) -> x 

let insert(x,h) = merge(h, NODE(0,x,EMPTY,EMPTY)) 

let deleteMin h = match h with 
| EMPTY -> raise EmptyHeap 
| NODE(_,x,lh,rh) -> merge (lh,rh) 

