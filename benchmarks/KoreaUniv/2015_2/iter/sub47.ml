let rec iter (n, (f:int->int)) p 
= match n with 
	0 -> p 
	|_ -> f (iter (n-1, f) p);; 