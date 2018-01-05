(*CSE/2011-11660/Kim Jiwoo/HW1-2*)
let rec sigma ((a:int), (b:int), (f:int->int)) : int = 
	 if (a == b) then f a
	 else if (a<b) then f a + sigma ((a+1),b,f) 
	 else 0