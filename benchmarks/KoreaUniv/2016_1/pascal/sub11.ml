let rec pascal : int * int -> int
= fun (a, b) -> if a < 0 || b < 0 then 0 else if a = b then 1 else if b = 0 then 1 else 
				pascal (a -1, b -1) + pascal(a -1, b) ;; 
