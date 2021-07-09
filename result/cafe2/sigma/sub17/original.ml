let rec sigma (f : int -> int) (a : int) (b : int) : int =
  let rec sum (f : int -> int) (n : int) : int =
    if n > 1 then f n + sum f (n - 1) else if n = 1 then 1 else 0
  in
  sum f b - sum f (a - 1)
