let rec iter ((n : int), (f : int -> int)) : int -> int =
  let a (f : int -> int) (b : int -> int) (x : int) : int = f (b x) in
  if n = 0 then fun (__s6 : int) -> __s6 else a f (iter (n - 1, f))


let (_ : int) = iter (2, fun (x : int) -> 2 + x) 0
