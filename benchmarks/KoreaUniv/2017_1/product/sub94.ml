
let rec product f a b =
if (a = b) then fun x -> f(a)
  else fun x -> f(a) * product f(x) (a+1) b;;
