let rec pascal : int * int -> int
= fun (n1, n2) -> (* TODO *)
	if n2 = 0 || n2 = n1 then 1
	else pascal (n1-1,n2-1) + pascal (n1-1,n2);;
