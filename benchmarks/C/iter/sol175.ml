let compose f g = fun x -> f (g x)

let rec iter (a, f) =
   if a = 0 then function x -> x
   else if a = 1 then f
   else if a < 0 then function x -> x
   else compose f (iter(a-1,f))


