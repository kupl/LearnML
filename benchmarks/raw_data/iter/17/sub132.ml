(*real code start*)
let rec iter ((n: int), (f: 'a->'a)) (x: 'a) : 'a =
 if (n<=0) then x
 else iter (n-1, f) (f(x))
(*real code end*)
