let rec func (n,f,a) = 
 match n with
0 -> a
| 1 -> f a
| x -> func(x-1,f,(f a));;

let rec iter (n,f) = fun a -> func(n,f,a);;
