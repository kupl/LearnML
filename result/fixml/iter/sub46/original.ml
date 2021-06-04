let rec iter : int * (int -> int) -> int -> int =
 fun (n, f) ->
  let a f b x = f (b x) in
  if n = 0 then f else a f (iter (n - 1, f))


let _ = iter (2, fun x -> 2 + x) 0
