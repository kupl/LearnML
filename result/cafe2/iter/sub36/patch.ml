let rec iter ((n : int), (f : int -> int)) : int -> int =
  let rec compose (f : int -> int) (g : int -> int) (x : int) : int = f (g x) in
  if n = 0 then fun (__s6 : int) -> __s6 else compose f (iter (n - 1, f))


let (_ : int) = iter (15, fun (x : int) -> x + 2) 0
