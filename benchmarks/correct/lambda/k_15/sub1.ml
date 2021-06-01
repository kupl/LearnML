type lambda = V of var
         | P of var * lambda
         | C of lambda * lambda
and var = string

let rec findTarget tar lst= 
match lst with 
|[] ->false
|hd::tl-> if hd=tar then true else findTarget tar tl;;

let rec isBound : ((var list) * lambda)->bool
=fun(x,y) -> match y with 
|V c -> findTarget c x
|P(c, d)-> isBound(c::x, d)
|C(c, d)-> isBound(x, c) && isBound(x, d);;
let rec check : lambda -> bool
=fun e -> isBound([], e);;
