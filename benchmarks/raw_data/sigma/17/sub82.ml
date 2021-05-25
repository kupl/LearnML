let rec sigma2 ((a: int), (b: int), (f: (int -> int)), (r: int)) : int =
     if a>b then 0
     else if a=b then (f a) + r
     else (sigma2 (a+1, b, f, (f a) + r))

let rec sigma ((a: int), (b: int), (f: (int -> int))) : int =
     sigma2 (a, b, f, 0)
