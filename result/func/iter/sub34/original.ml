let rec iter ((n : int), (f : int -> int)) : int -> int =
  let rec compose (f : int -> int) (g : int -> int) (x : int) : int = f (g x) in
  if n = 0 then f else compose f (iter (n - 1, f))


let (_ : int) = iter (0, fun (x : int) -> 2 + x) 0

let (_ : int) = iter (1, fun (x : int) -> 2 + x) 0

let (_ : int) = iter (2, fun (x : int) -> 2 + x) 0

let (_ : int) = iter (3, fun (x : int) -> 2 + x) 0
