
let rec product f a b =
  match (b - a) with
  |0 -> f b
  |_ -> (f a) * (product f (a+1) b)