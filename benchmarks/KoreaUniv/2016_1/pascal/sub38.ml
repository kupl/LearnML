let rec pascal (n1, n2) = if (n1<0||n2<0) then failwith "error : pascal's elements (n1<0 or n2<0)"
											else if (n1<n2) then failwith "error : pascal's elements (n1<n2)"
													else if n2 = 0 then 1
													else if n2 = n1 then 1
													else pascal(n1-1, n2-1) + pascal(n1-1,n2);;
