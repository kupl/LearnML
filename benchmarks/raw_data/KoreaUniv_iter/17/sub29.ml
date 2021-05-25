(* problem 3*)
  let iter : int * (int -> int) -> (int -> int)
  = fun (n,f) ->
  let rec compose : int * (int -> int) * (int -> int) -> int -> int
  = fun (n,f,g) -> if n=0 then fun x -> g(x)
  else compose(n-1, f, fun x-> g(f(x))) in
  compose(n, f, fun x -> x);;