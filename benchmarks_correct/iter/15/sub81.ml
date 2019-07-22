let rec iter = fun (n,f) ->
	if(n>0) then
		let composition x = iter(n-1,f)(f x) in
		composition
	else let iden x = x in
		iden
(*let func x = (x*x)-1
let ans = iter(3,func) 2
let _= print_endline(string_of_int ans)*)
