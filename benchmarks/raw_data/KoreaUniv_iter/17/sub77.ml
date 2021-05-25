(*3*)
let rec iter : int * (int->int)->(int->int)=fun (n,f)->
if n>1 then fun x->f(iter((n-1),f) x) else if n=1 then  fun x->f(x) else fun x->x;; 
