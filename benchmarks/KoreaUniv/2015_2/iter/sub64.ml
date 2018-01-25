let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> 
  match (n,f) with
  | (0, _) -> fun x -> x 
  | (1, _) -> f
  | _ -> fun x -> iter(n-1, f) (f x)
