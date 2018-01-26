let rec fib n = if n = 0 then 0
								else if n = 1 then 1
								else if n < 0 then failwith "error: fib's element (n<0)"
								else fib (n-1) + fib (n-2);;
