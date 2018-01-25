let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) num -> 
  match (n,f) with
  |(0,_) -> 0
  |(1,_) -> f num
  |(_,_) -> if n>1 then let num = f num in iter(n-1,f) num
  else 0
