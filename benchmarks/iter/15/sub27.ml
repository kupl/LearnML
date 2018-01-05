let rec iter(n, f) x = if n<=0 then x
					else iter(n-1, f) (f x)

(*TESTCASE
let twon n = iter(n, fun x -> 2+x) 0

let _ = print_endline( string_of_int(twon 30))
*)
