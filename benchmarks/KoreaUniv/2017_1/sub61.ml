(*#1*)
let rec fastexpt : int -> int-> int = fun b n-> 
  if n < 0 then raise (Failure "n is negative number")
  else if n = 0 then 1
  else if n mod 2 = 0 then (fastexpt b (n/2)) * (fastexpt b (n/2))
  else b * (fastexpt  b (n-1));;


(*#2*)
let smallest_divisor : int -> int = fun n ->
  if n < 0 then raise (Failure "n is negative number")
  else if n mod 2 = 0 then 2
  else let x = int_of_float (sqrt (float_of_int n)) in
    let rec f i n =
      if i > x then n
      else if n mod i = 0 then i
      else (f (i+2) n) in f 3 n;;


(*#3*)
let rec iter : int * (int -> int) -> (int -> int) = fun (n,f) ->
  if n < 0 then raise (Failure "n is negative number")
  else if n = 0 then (fun x -> x)
  else fun x -> f(iter(n-1,f)x);;


(*#4*)
let rec product : (int -> int) -> int -> int -> int = fun f a b ->
  if a > b then raise (Failure "a is bigger than b")
  else if a = b then f b
  else (f a) * (product f (a+1) b);;


(*#5*)
let rec dfact : int -> int = fun n ->
  if n < 0 then raise (Failure "n is negative number")
  else if n = 1 || n = 0 then 1
  else if n = 2 then 2
  else n * (dfact (n-2));;



(*#6*)
let rec drop : 'a list -> int -> 'a list = fun l n ->
  match l with
  |[] -> []
  |hd::tl -> if n < 1 then l
             else drop tl (n-1);;
 

(*#7*)
let rec unzip : ('a * 'b) list -> 'a list * 'b list = fun lst ->
  let f1 (x,_) (a,_) = x::a
    in let f2 (_,y) (_,b) = y::b
      in match lst with
      |[] -> ([],[])
      |(x,y)::tl -> (f1 (x,y) (unzip tl)),(f2 (x,y) (unzip tl));;



(*#8*)
let rec change : int list -> int -> int = fun coins amount ->
  if amount = 0 then 1
  else if amount < 0 then 0
  else match coins with
  |[] -> 0
  |hd::tl -> if hd < 1 then raise (Failure "a coin is smaller than 1")
             else(change tl amount) + (change coins (amount-hd));;
