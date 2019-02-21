let rec max : int list -> int
=fun l -> 
	match l with
	|[] -> 0
  |x::[] -> x
	|h::t ->
		let v = max t in
		if h > v then h else v
 
let rec min : int list -> int
=fun l -> 
	match l with
	|[] -> 0
	|x::[] -> x
	|h::t ->
		let v = min t in
		if h < v then h else v
