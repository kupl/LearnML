
let rec drop : 'a list -> int -> 'a list = fun l n ->
	if n<0 then raise (Failure "n is negative number.")	
	else if n=0 then l else (match l with [] -> [] |hd::tl -> drop tl (n-1));;
