(*CSE/2011-11660/Kim Jiwoo/HW1-1*)
let rec merge ((x:int list), (y:int list))  :  int list = 
	match x, y with
	[], [] -> []
	| h::t, [] -> x
	| [], h::t -> y
	| h1::t1, h2::t2 -> if (h1<h2) then h2::(merge (x,t2)) else h1::(merge (t1,y))