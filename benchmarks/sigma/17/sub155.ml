let sigma : int * int * (int -> int) -> int = fun (a,b,f) ->
	if a>b then 0 else
	let sum = ref 0 in
	for i=a to b do 
		sum := !sum + f i
	done;
	!sum
