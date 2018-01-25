
(*problem4*)
let rec product : (int ->int)-> int -> int ->int = fun f a b->
if(b<a) then raise (Failure "error : b is smaller than a") 
else if (a=b) then (fun x -> x) (f a)
else (product f b b)*(product f a (b-1));;
