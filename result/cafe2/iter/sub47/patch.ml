let rec iter ((n : int), (f : int -> int)) : int -> int =
  let compose (f : int -> int) (g : int -> int) (x : int) : int = f (g x) in
  if n != 0 then fun __s5 -> iter (n - 1, f) (f __s5)
  else if n = 0 then fun __s4 -> __s4
  else raise Failure "Exception(Template)"


let (_ : int) = iter (6, fun (x : int) -> 2 + x) 0
