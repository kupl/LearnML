let rec pascal(n1, n2) = 
 match (n1, n2) with 
 | (n1, 0) -> 1
 | (1, 1) -> 1
 | _ ->
 if n1=n2 then 1
 else pascal(n1-1, n2-1) + pascal(n1-1, n2)
