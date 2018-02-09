let ans  = [];;
let rec filter pred lst =
		match lst with
		| [] -> ans
		| hd::tl -> if pred hd then hd::filter pred tl
							  else filter pred tl
