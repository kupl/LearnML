let rec iter(n,f) x = 
   match n with
| 0 -> x
| 1 -> f x
| i -> f (iter(i-1, f) x)
