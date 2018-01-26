let rec pascal : int * int -> int
= fun (n1, n2) -> if n1<0 || n2<0 then 0
	else if n2>n1 then 0
	else if n2=0 || n1=n2 then 1
	else pascal (n1-1,n2-1) + pascal (n1-1,n2)
