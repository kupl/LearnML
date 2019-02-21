(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> 0 


let rec max : int list -> int
= fun lst ->
 let rec rmax : int -> int list -> int
= fun m l ->
	match l with
		[] -> m
		| x::xs -> if m > x then rmax m xs else rmax x xs  
	in
		match lst with
		x::xs -> rmax x xs;;



let rec min : int list -> int
= fun lst -> 
	let rec rmin : int -> int list -> int
= fun m l ->
	match l with
		[] -> m
		| x::xs -> if x > m then rmin m xs else rmin x xs
	in
		match lst with
		x::xs -> rmin x xs;;
