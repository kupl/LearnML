let rec iter : int * (int -> int) -> int -> int =
 fun (n, f) ->
  let rec compose f g x = f (g x) in
  if n = 0 then f else compose f (iter (n - 1, f))


let _ = iter (0, fun x -> 2 + x) 0

let _ = iter (1, fun x -> 2 + x) 0

let _ = iter (2, fun x -> 2 + x) 0

let _ = iter (3, fun x -> 2 + x) 0
