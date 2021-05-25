let rec prime : int -> bool
= fun n -> (*TODO*)
let rec f = fun (n, x) ->
if x = 1 then true
else if n mod x = 0 then false
else f (n , x-1) 
in f (n, n-1);;
  
