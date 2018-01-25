(* problem 8*)
let change : int list -> int -> int
= fun coins amount ->
	if amount<0 then 0
	else match coins with
		|[]-> 0
		|hd::tl -> (*let coins = sort coins in *)
		
	let result = Array.make (amount + 1) 0 in
	Array.set result 0 1;

	let changes a coin = 
		for i = 1 to ((Array.length a)-1) do
			if ((i-coin)>=0) then Array.set a i ((Array.get a i) +(Array.get a (i-coin)))
		done in

	for i = 0 to ((List.length coins)-1) do
		changes result (List.nth coins i)
	done;
	Array.get result amount;;




	  



