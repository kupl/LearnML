(*problem3*)
let rec prime n
=let rec help d
	=d*d>n || (n mod d<>0 && help(d+1)) in n<>1 && help 2
	
