let rec iter : int * (int -> int) -> int -> int =
 fun (n, f) ->
  let f1 x = if n = 0 then f (x - 2) else f (x + 2) in
  if n = 0 then f1 else iter (n - 1, f1)
