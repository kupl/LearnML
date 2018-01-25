  (*Problem 4*)
let rec product f a b =
if a<b then (f b)*(product f a (b-1))
else if a>b then (f a)*(product f (a-1) b)
else f a;;
