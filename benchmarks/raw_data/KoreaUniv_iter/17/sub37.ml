(* problem 3*)

let rec iter : int * (int -> int) -> (int -> int) 
= fun (n, f) ->
   if n < 0 then raise (Failure "n has to be positive") (*exclude the case when n is negative*)
   else
       match n with (*there are 2 cases; n is 0 or not*)
       0 -> fun x -> x (*when n is 0, return the identity function*)
       |_ -> (*when is a value other than 0, need to make a sub function "compose_func" for composing functions*)
	  let compose_func f g x = f(g x) in compose_func f (iter ((n - 1), f));;
	  (*The function "compose_func" is mapped because the fucntion "iter" is a function that composes itself. And we need to call the function "iter" recursively til n becomes 0*)