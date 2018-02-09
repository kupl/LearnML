let rec fold f l a = 
		match  l with
		[]-> a
		|hd::tl -> f hd (fold f tl a)

let rec filter pred lst = 
	match lst with
		[]-> []
		|hd::tl-> fold(fun x y -> if (pred x) then [x]@y else y) lst []
