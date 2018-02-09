let rec filter pred lst = 
	let rec loop i o =
		match i with
		|	[] -> o
		|	h :: t -> loop t (if pred h then h :: o else o) in
	let rec reverse i o =
		match i with
		|	[] -> o
		|	h :: t -> reverse t (h :: o) in
	reverse (loop lst []) []
