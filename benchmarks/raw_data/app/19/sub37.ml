let rec slice l i k = match l with
 h::t -> if (k = 0) then h 
 else if (i = 0) then h @ (slice t 0 (k-1)) 
 else slice t (i-1) (k-1);;