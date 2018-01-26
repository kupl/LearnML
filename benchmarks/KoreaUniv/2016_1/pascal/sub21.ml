let rec pascal : int * int -> int
= fun (n1, n2) -> 
	if n1<n2 || n1<0 
		then -1 
	else if n2=0 || n1=n2 
		then 1 
	else 
		pascal (n1-1, n2) + pascal (n1-1, n2-1)
