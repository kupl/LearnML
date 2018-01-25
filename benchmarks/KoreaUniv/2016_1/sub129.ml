(* Problem 1 *)
 let rec fib : int -> int
	= fun n -> match n with
	| 0 -> 0
	| 1 -> 1
	| n -> fib(n-1) + fib(n-2)

(* Problem 2 *)
 let rec pascal: int * int -> int
 = fun(x,y) ->
   if y =0 || y = x then 1
   else pascal(x-1,y-1) + pascal(x-1,y)

(* Problem 3 *)
 let rec  prime: int -> bool
  =fun n ->
    let n = n in
     let rec div k =
     if n = 1 then false
    else if  n = k ||  n / k < k then true
    else n mod k != 0 && div(k+1) in
      div 2;;

(* Problem 4 *)
 let rec sigma : (int -> int) -> int -> int ->int
 = fun f a b ->
   if a=b then f(a)
   else f(a) + sigma(f)(a+1)(b)

