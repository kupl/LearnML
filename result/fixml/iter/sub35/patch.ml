let rec iter : int * (int -> int) -> int -> int =
 fun (n, f) ->
  if n = 0 then fun __x__ -> __x__ else fun x -> iter (n - 1, f) (f x)


let _ = iter (2, fun x -> 2 + x) 0
