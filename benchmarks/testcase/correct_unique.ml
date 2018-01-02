{
	[5;6;5;4] -> [5;6;4];
	[3;5;7;5;7;4;8] -> [3;5;7;4;8];
}
let rec f lst = 
	match lst with
	|[] -> []
	|hd::tl -> 
		let rec sub_f elem l =
		(match l with
		|[] -> true
		|h::t -> if(h=elem) then false else sub_f elem t)
	in if (sub_f hd tl) then hd::(f tl) else (f tl);;