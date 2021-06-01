(*CSE/2011-11660/Kim Jiwoo/HW1-2*)
let rec sigma f a b =
	 if (a == b) then f a
	 else if (a<b) then f a + sigma f (a+1) b
	 else 0