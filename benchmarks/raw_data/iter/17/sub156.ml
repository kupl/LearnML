let rec iter( (n : int), (f : int -> int) ) x =
  match n with
   0 -> x
  | _ -> iter(n-1,f) (f x)