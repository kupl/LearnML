let rec pascal (n1, n2) = if n2 = 0 then 1
else if n1 = n2 then 1
else if n2 > n1 then raise (Failure "Error")
else pascal(n1-1, n2-1) + pascal(n1-1, n2);;
