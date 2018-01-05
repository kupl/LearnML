type heap = EMPTY | NODE of rank * value * heap * heap 
and rank = int 
and value = int 

exception EmptyHeap 

let rank h = match h with 
  | EMPTY -> -1 
  | NODE(r,_,_,_) -> r 

let shake (x,lh,rh) =   (*chage LEFT RIGHT*)
  if (rank lh) >= (rank rh) 
  then NODE(rank rh+1, x, lh, rh) 
  else NODE(rank lh+1, x, rh, lh) 


let rec merge( heap1, heap2) =
		match (heap1, heap2) with
		| (EMPTY,_) -> heap2
		| (_, EMPTY) -> heap1
		| (NODE(_,x1,rh1,lh1), NODE(_,x2,rh2,lh2 ) )-> ( if( x1 > x2 ) then shake (x2,merge(rh2,heap1),lh2)
		 											else shake(x1,merge(rh1,heap2),lh1) )
		
let findMin h = match h with 
  | EMPTY -> raise EmptyHeap 
  | NODE(_,x,_,_) -> x 

let insert(x,h) = merge(h, NODE(0,x,EMPTY,EMPTY)) 

let deleteMin h = match h with 
  | EMPTY -> raise EmptyHeap 
  | NODE(_,x,lh,rh) -> merge (lh,rh) 



	(*
 
  x.rightChild = merge(x.rightChild, y);
 
  if(x.leftChild == null) {
    // left child doesn't exist, so move right child to the left side
    x.leftChild = x.rightChild;
    x.rightChild = null;
 
  } else {
    // left child does exist, so compare s-values
    if(x.leftChild.s < x.rightChild.s) {
      Node temp = x.leftChild;
      x.leftChild = x.rightChild;
      x.rightChild = temp;
    }
    // since we know the right child has the lower s-value, we can just
    // add one to its s-value
    x.s = x.rightChild.s + 1;
  }
  return x;
}*)
