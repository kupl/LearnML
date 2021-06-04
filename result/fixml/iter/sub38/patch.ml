let rec iter : int * (int -> int) -> int -> int =
 fun (n, f) ->
  let compose f g x = f (g x) in
  if n <= 1 then if n = 1 then f else fun __x__ -> __x__
  else compose f (iter (n - 1, f))


let _ = iter (0, fun x -> 2 + x) 0

let _ = iter (1, fun x -> 2 + x) 0

let _ = iter (5, fun x -> 2 + x) 0
