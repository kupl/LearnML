let rec sigma (a, b, f) = if a>b then 0 
						else (f a) + sigma (a+1, b, f)

(*TESTCASE
 let _ = print_endline (string_of_int (sigma(4, 4, fun x -> 2*x))) *)
