let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> match n with
  0 -> fun y -> y
  |_ -> fun x -> f(iter(n - 1, f) x);;