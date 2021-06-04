let __s1 (__s2 : int) = __s2

let rec iter ((n : int), (f : int -> int)) : int -> int =
  let compose (f : int -> int) (g : int -> int) (x : int) : int = f (g x) in
  if n = 0 then __s1 else compose f (iter (n - 1, f))


let (_ : int) = iter (0, fun (x : int) -> 2 + x) 0

let (_ : int) = iter (1, fun (x : int) -> 2 + x) 0

let (_ : int) = iter (5, fun (x : int) -> 2 + x) 0
