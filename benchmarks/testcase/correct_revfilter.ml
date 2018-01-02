{
	fun a -> (a>=6), [5;4;8;9;3;9;0;2;3;4;5;6;61;2;3;4] -> [61;6;9;9;8];
}
let f check lst = 
	match lst with
	|[] -> []
	|hd::tl ->
	  if(check hd) then (f check tl)@[hd]
		else f check lst;;