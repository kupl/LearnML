let rec sigma f a b =
  let rec sum f n =
    if a < n + 1 then f n + sum f (n - 1) else if n = 1 then 1 else 0
  in
  sum f b - sum f (a - 1)
