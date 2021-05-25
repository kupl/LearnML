
(* Exercise 3*)
let rec iter = fun (n,f) ->
	if n <= 0 then (fun x -> x)
	else (fun x -> (iter (n-1 , f) (f x)))
