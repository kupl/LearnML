let rec iter : int * ('a -> 'a) -> ('a -> 'a) = fun (n, f) ->
	let _f : 'a -> 'a = fun(_a) -> f(iter(n-1,f)(_a)) in
	if(n>0)	then _f
	else let _f : 'a -> 'a = fun(_a) -> _a in _f 
(*
let _ =
	let f : int -> int = fun(n) -> n+1 in
	let f2 : int -> int = fun(m) -> 2*m in
	print_int(iter(3,f)(1));
	print_int(iter(10,f2)(1));
*)
