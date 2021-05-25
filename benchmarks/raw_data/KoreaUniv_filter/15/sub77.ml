
let rec filter p l =
	match l with
	[]->[]
	|hd::tail->if (p hd) = true then hd::(filter p tail)
		else (filter p tail)