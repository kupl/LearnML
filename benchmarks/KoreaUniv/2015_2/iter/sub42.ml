let id = fun x -> x
let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) id->
  match n with
  |0 -> id
  |_ -> f (iter(n-1,f) id);;

