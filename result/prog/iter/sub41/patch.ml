let __s8 __s9 = __s9

let rec iter ((n : int), (f : int -> int)) : int -> int =
  let compose (f : int -> int) (g : int -> int) (x : int) : int = f (g x) in
  if n = 0 then __s8 else compose f (iter (n - 1, f))


let (_ : int) = iter (7, fun (x : int) -> 2 + x) 0
