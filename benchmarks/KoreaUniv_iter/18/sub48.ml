let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> (*TODO*)
match n with
  | 1 -> f
  | _ -> fun x->f(iter(n-1,f) x);;
  
iter(2,fun x->2+x) 0;;
iter(3,fun x->2+x) 0;;
iter(4,fun x->2+x) 0;;
iter(2,fun x->2+x) 1;;
iter(3,fun x->2+x) 1;;
iter(4,fun x->2+x) 1;;