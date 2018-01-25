
let product : (int->int) -> int -> int -> int
  = fun f a b -> let rec prod f a b = if a = b+1 then 1 else (f a)*(prod f (a+1) b) in prod f a b