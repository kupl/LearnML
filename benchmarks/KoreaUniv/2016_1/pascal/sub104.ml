let rec pascal (n1, n2) =
 if n2 = 0 || n1 = n2 then 1
 else pascal (n1-1, n2-1) + pascal (n1-1, n2);;
