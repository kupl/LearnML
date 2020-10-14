let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> 
  let compose f g = (fun x -> f(g(x)) )in
  match n with
    0->(fun x->x)
    |1->f
    |_->let h = iter(n-1,f) in (compose f h);;
    
    
