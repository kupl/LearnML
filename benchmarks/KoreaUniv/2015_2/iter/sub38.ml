let comp f g = (fun x -> f(g x))


let rec loop : int*(int -> int)*(int -> int) -> (int -> int)
= fun (x,y,z)->
if x!=1 then loop(x-1,y,comp z y) else z


let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> 
match n with
|0 -> f
|_ -> loop(n,f,f)

