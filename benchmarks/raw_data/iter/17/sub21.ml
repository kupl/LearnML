let rec iter ((n:int), (f:'a->'a)):('a->'a)=
  match n with
  |0 -> fun x -> x
  |_ -> fun x -> (iter ((n-1), f)) (f x) 
  
