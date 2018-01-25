(* Problem 1 *)
let rec fib : int -> int
= fun n -> 0 (* TODO *)
(*HW 1*)
let is_Zero n = if n=0 then true else false;;
let is_One n = if n=1 then true else false;;
let check n = if n=1 then 1 else 0;;
let rec fib n = if (is_Zero n) || (is_One n) then check n else (fib(n-1) + fib(n-2));;


(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> 0 (* TODO *)
(*HW 2*)
exception Problem;;

let rec pascal (x, y) =
	if x<0 || y<0 then raise Problem
	else if x < y then raise Problem
	else if y=0 then 1
	else if x=y then 1
	else pascal(x-1, y-1) + pascal(x-1, y);;


(* Problem 3 *)
let rec prime : int -> bool
= fun n -> true (* TODO *)
(*HW 3*)
let under_Two n = if n<2 then true else false;;
let is_Two n = if n=2 then true else false;;

let rec prime n = 
	if under_Two n then false
	else if is_Two n then true
	else	
		let rec innerFun k =
				(n mod k <> 0 && innerFun(k+1)) || k*k>n
				in innerFun 2 && n<>1;;




(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> 0 (* TODO *)
(*HW 4*)
let rec sigma f a b = 
  	if f a <> f b then let induction = f b in induction + sigma f a (b-1) 
  	else f b ;;    

let rec factorial n = 
  if(?) then 1
  else n

let rec factorial n = 
  if(?) then 1
  else n*factorial(n-1)

let rec map func lst =
	match lst with
	|[] -> []
	|hd::tl -> (((func hd)+1)*3)::(map func tl)  

let rec map func lst =
	match lst with
	|[] -> []
	|hd::tl -> (hd)::(?)  

func fib(int n)
{
	if (n=0) then return 1;
	else if (n=1) then return 1;
	else return fib(n-1)+fib(n-2);
}

fib[n];
fib[0]=1; fib[1]=1;
for(i=2;i<=n;i++)
{
	fib[i]=fib[i-1]+fib[i-2];
}
return fib[n];

let g n = 4 * n
let f n= ?

let rec f lst n =
	if(n<0 || lst=[]) then 0
	else if (n=0) then 1
	else 
	let rec sub_f lst =
	match lst with
	|[] -> 0
	|hd::tl -> (f lst (n-hd)) + (sub_f tl)
  in (sub_f lst)