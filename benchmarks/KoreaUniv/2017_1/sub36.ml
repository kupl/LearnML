(* problem 1*)
  let rec f b n = 
  if n = 0 then 1 
  else b * (f b ((n-1)/2)) * (f b (n/2))


(* problem 2*)
  let rec div n x = if n mod x = 0 then x
  else div n (x+2)
  let smallest_divisor : int -> int
  = fun n -> if n = 0 then 0
  else if n = 1 then 1
  else if n mod 2 = 0 then 2
  else div n 3

  (* problem 3*)
  let rec iter : int * (int -> int) -> (int -> int)
  = fun (n,f) -> match n with 
  | 1->f
  |_ -> iter ((n-1),f)

  (* problem 4*)
  let rec product : (int -> int) -> int -> int -> int
  = fun f a b -> if b = a then b
  else b * product f a (b-1)

  (*problem 5*)
  let rec dfact :int -> int
  =fun n -> if n =1 || n =2  then n
  else n * dfact (product ( fun x-> x) (n-2) (n-2))

  (*problem 6*)
  let rec drop : 'a list -> int -> 'a list
  = fun l n -> match l with
  | [] -> []
  | hd :: tl -> if n = 1 then tl
  else (drop tl (n-1))

  (*problem 7*)
  let rec unzip: ('a * 'b) list -> 'a list * 'b list
  = fun lst -> match lst with
  |[] -> ([],[])
  |(x,y) :: tl ->
  let (fst, sec) = unzip tl in 
  (x:: fst, y::sec)
  
  (*problem 8*)
  let rec change : int list -> int -> int
  =fun coins amount -> 
  match coins with 
  [] -> 0
  |hd :: tl -> (fun x -> 
      match x with 
      |0 -> 1 
      |_ -> (if amount > 0 then (if (amount/hd)>0 then ((change coins (amount-hd)) + (change tl amount)) else change tl amount) else 0)) amount


