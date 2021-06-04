let rec iter ((n : int), (f : int -> int)) : int -> int =
  let rec compose (g : int -> int) (f : int -> int) (x : int) : int = g (f x) in
  if n != 0 then fun __s5 -> iter (n - 1, f) (f __s5)
  else if n = 0 then fun __s4 -> __s4
  else raise Failure "Exception(Template)"


let (_ : int) = iter (3, fun (x : int) -> 2 + x) 0
