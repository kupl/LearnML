let rec iter : int * (int -> int) -> int -> int =
 fun (n, f) ->
  let rec compose f g x = f (g x) in
  if n < 2 then if n = 1 then f else fun __x__ -> __x__
  else compose f (iter (n - 1, f))


let _ = iter (15, fun x -> x + 2) 0
