(*Lee Seok Jin 2013-11417 CSE hw1_2 *)

let rec sigma((a,b,f): int*int*(int->int)): int = 
	if(a>b) then 0 
	else f(a)+sigma(a+1,b,f)
