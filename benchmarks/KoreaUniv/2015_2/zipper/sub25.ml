let rec zipper : int list * int list -> int list
=fun (a,b) ->
  match (a,b) with
  |(_, []) -> a
  |([], _) -> b
  |(h1::t1,h2::t2) -> 
  if h1>h2 then zipper(h2::a,t2)
  else if h1<h2 then h1::zipper(t1,b)
  else zipper(a, t2)
