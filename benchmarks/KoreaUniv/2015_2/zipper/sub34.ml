let rec zipper : int list * int list -> int list
=fun (a,b) ->
match a with
|[] -> b
|hd::tl -> 
	(match b with
	|[] -> a
	|hd2::tl2->[hd;hd2]@zipper(tl,tl2));;
