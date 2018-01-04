(*2006-11720 Kim Eunsol HW1 #3*)

let rec zipper(a,b) = 
	match a with aa::aaa -> (
			match b with [] -> aa::aaa
			|bb::bbb -> aa::bb::zipper(aaa,bbb)
			)
	|[] -> (
			match b with [] -> []
			|bb::bbb -> bb::bbb
		   )
