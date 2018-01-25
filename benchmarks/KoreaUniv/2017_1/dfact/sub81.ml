(*problem 4*)
  let rec product:(int->int)->int->int->int
  =fun f a b->
  if b=a then f a
  else (f b)*(product f a (b-1));;

(*problem 5*)
  let rec dfact:int->int
  =fun n->
  match n with
  |1->1
  |_->(product (fun x->x) 1 n)/dfact (n-1);;
