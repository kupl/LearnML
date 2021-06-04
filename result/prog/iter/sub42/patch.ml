let __s8 __s9 = __s9

let rec iter ((n : int), (f : int -> int)) : int -> int =
  let rec compose (g : int -> int) (f : int -> int) (x : int) : int = g (f x) in
  if n < 0 then f else if n = 0 then __s8 else compose f (iter (n - 1, f))


let (_ : int) = iter (3, fun (x : int) -> 2 + x) 0
