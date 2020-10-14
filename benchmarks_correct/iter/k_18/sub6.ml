let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) ->
match (n, f) with
  |(0, f) -> fun x -> x
  |(1, f) -> f
  |(n, f) -> 
    fun x-> 
    f(iter(n-1, f) x);;
  
  iter(4, fun x -> 1+x) 0;;


(*
When n = 0, 
iter(n, f) is defined to be the identity function. 
When n > 0,
iter(n, f) is the function 
that applies f repeatedly n times. 

For instance,
iter(n, fun x -> 2+x) 0
evaluates to 2 × n.
*)