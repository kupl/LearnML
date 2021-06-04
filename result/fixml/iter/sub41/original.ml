let rec iter : int * (int -> int) -> int -> int =
 fun (n, f) ->
  let compose f g x = f (g x) in
  if n <= 1 then f else compose f (iter (n - 1, f))


let _ = iter (7, fun x -> 2 + x) 0
