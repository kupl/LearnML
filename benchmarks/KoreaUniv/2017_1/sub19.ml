(* problem 1*)
  let rec  fastexpt : int -> int -> int
  = fun b n -> let square a = a * a in if n = 0 then 1 
  else if n mod 2 = 0 then square (fastexpt b (n/2))
  else b * (fastexpt b (n-1));;

  (* problem 2*)

  let smallest_divisor : int -> int
  = fun n -> let rec num n i = if n = 1 then 1
  else if (n mod i = 0 ) then i
  else num n (i+1) in num n 2;;

  (* problem 3*)

  let rec  iter : int * (int -> int) -> (int -> int)
  = fun(n,f) -> if n = 0 then fun x-> x*1
  else fun x -> iter(n-1,f) (f x);;

  (* problem 4*)

  let rec  product : (int -> int) -> int -> int -> int
  = fun f a b -> let f = fun x -> x * 1 in if a> b then 1
  else (f a) * product f (a+1) b

  (* problem 5*)

  let rec  dfact : int -> int
  = fun n -> if n = 1 then 1
  else if n = 2 then 2 else n * dfact (n-2);;

  (* problem 6*)

  let rec drop : 'a list -> int -> 'a list
  = fun l n -> if n = 0 then l else drop (match l with |[] -> [] |hd::tl -> tl) (n-1);;

  (* problem 7*)

  let rec unzip : ('a * 'b) list -> 'a list * 'b list
  = fun lst -> match lst with | [] -> ([],[])
  |(x,y)::tl -> let lst1,lst2 = unzip tl in x::lst1,y::lst2;;

  (* problem 8*)

  let rec change : int list -> int -> int
  = fun coins amount -> let zero = 0 in if amount = 0 then 1
  else if amount < zero then 0
  else match coins with
  |[] -> 0
  |hd::tl -> change coins (amount-hd) + change tl amount;;

