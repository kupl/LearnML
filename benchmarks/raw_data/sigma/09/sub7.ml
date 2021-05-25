exception Error of string;;
let rec sigma (a,b,f) = 
		if a < b then f( a ) + sigma(a+1,b,f)
		else if (a==b) then f(a)
		else raise (Error "a should not be greater than b!");;
