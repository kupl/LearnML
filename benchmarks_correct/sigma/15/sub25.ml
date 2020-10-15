let rec sigma f a b  = if a>b then 0 
						else (f a) + sigma f (a+1) b

(*TESTCASE
 let _ = print_endline (string_of_int (sigma(4, 4, fun x -> 2*x))) *)
