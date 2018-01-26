let rec prime : int -> bool
= fun n ->
	let i = ref 2 in
	let temp = ref 1 in
	while not (!temp = 0) do
		temp := n mod !i; 
		i := !i + 1;
	done;
	if !i = n+1 then true
	else false;;
		