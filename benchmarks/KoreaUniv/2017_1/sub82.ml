 
  (* problem 1*)
  let rec fastexpt : int -> int -> int
  = fun b n ->
   if n = 0 then 1
   else if n mod 2 = 0 then (fastexpt b (n/2))*(fastexpt b (n/2))
   else b*(fastexpt b (n-1))

  (* problem 2*)

  let smallest_divisor : int -> int
  = fun n -> 
    let rec asdf n k =
    if n mod k = 0 then k
    else if k>(n/2) then n
    else asdf n (k+1)
    in asdf n 2

  (* problem 3*)

   

  (* problem 4*)

  let rec product : (int -> int) -> int -> int -> int
  = fun f a b -> 
  if a<b then (product f (a+1) b)*f(a)
  else f(a)
  (* problem 5*)

  let rec dfact : int -> int
  = fun n -> 
    if n=0 then 1
    else if n=1 then 1
    else n*(dfact(n-2))

  (* problem 6*)

  let rec drop : 'a list -> int -> 'a list
  = fun l n -> 
  match l with
  |[]->[]
  |hd::tl -> if n>0 then (drop tl (n-1)) else l
  
  (* problem 7*)
  let unzip : ('a*'b)list->'a list*'b list
  =fun list ->
  List.split list
  
  (* problem 8*)

