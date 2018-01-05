let rec sigma(a, b, f) = 
  if a > b then 0 
  else f(a) + sigma(a+1, b, f)

(* base case 1 *)
let t1 = (10 == sigma (10, 10, fun x -> x))
let t2 = (sigma(11,10, fun x->x) == 0)
let t3 = (sigma(1,10,fun x->x) == 55)
let t4 = (sigma(1,10,fun x->if x mod 2 = 0 then 1 else 0) == 5)
let t5 = (sigma(1,10, fun x-> x * x) == 385)
;;

