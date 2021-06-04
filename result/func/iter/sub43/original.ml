let iter ((n : int), (f : int -> int)) (m : int) : int =
  let rec loop (n : int) (f : int -> int) : int =
    if n < 2 then f m else f m + loop (n - 1) f
  in
  loop n f


let (_ : int) = iter (12, fun (x : int) -> 2 + x) 2
