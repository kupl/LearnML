(*#3*)
let rec iter : int * (int -> int) -> (int -> int) = fun (n,f) ->
  if n < 0 then raise (Failure "n is negative number")
  else if n = 0 then (fun x -> x)
  else fun x -> f(iter(n-1,f)x);;