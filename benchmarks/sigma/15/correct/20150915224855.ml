let rec sigma : int*int*(int->int)->int = fun (a,b,f)->
 if a>=b then f(b)
 else f(a)+sigma(a+1,b,f)