(*problem 3*)
  let compose (f:int->int) (g:int->int)  (x:int) : int = f (g x);;
  let rec iter:int*(int->int)->(int->int)
  =fun(n,f)->
  if n=0 then fun x->x
  else compose f (iter((n-1),f));;
