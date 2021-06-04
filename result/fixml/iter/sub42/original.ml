let rec iter : int * (int -> int) -> int -> int =
 fun (n, f) ->
  let rec compose g f x = g (f x) in
  if n = 0 || n = 1 then f else compose f (iter (n - 1, f))


let _ = iter (3, fun x -> 2 + x) 0
