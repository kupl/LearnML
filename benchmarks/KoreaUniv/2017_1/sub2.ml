(*1*)
  let rec fastexpt : int -> int -> int
  = fun b n ->
  if n=0 then 1
  else if n =1 then b 
  else if n mod 2 = 1 then b*fastexpt b (n-1)
  else (fastexpt b (n/2))*(fastexpt b (n/2));;


(*2*)
  let smallest_divisor : int -> int
  =fun n ->
  let rec divisor n m=
  if m>(n/2) then n
  else if (n mod m=0) then m
  else divisor n (m+1) in divisor n 2;;

(*3*)
  let rec iter: int * (int -> int)->(int ->int)
  =fun (n,f) ->
    if n=0 then iter(1 , fun x ->x) 
    else if n=1 then f
    else fun x-> f(iter((n-1),f)x) ;;

(*4*)
  let product : (int -> int) -> int -> int -> int
  =fun f a b ->
    let rec prod n=
      if n=1 then 1 else n*prod(n-1)
  in 
  f(prod b)/f(prod a)
  ;;

(*5*)
  let dfact :int -> int
  =fun n ->
    if n mod 2 =0 then product (fun x -> 2*x) 2 n/2
    else product (fun x->2*x-1) 1 n/2 ;;

(*6*)
  let drop : 'a list -> int -> 'a list
  =fun l n ->
    let rec length l=
      match l with 
      | [] -> 0
      | hd::tl -> 1+length tl
      in
    let rec remove l n=
    match l with
    |[] -> []
    |hd::tl-> if n=1 then tl else remove tl (n-1)
    in
  if length l<=0 then [] else remove l n;;

(*7 there seems to be a error in the type 
  let unzip : ('a*'b)list -> 'a list *'b list
  =fun lst ->
    let rec zip lst=
    match lst with
    |[]->[]
    |(a,b)::tl->[a::zip tl],[b::zip tl] 
    in zip lst
  ;;
*)
(*7*)
  let unzip : ('a*'b)list -> 'a list *'b list
  =fun lst ->
  List.split lst
  ;;
    
(*8*)

  let change : int list -> int -> int
  =fun coins amount ->
  
  (*let  rec nth l n =
   match l with 
   |[]->raise
   |hd::tl -> if n=0 then hd else nth tl (n-1) 
  in
  *)
  let rec length l=
  match l with
  |[]->0
  |hd::tl-> 1+length tl
  in
 
  let amount = amount -( amount mod 5) in
   let rec counter coins amount n =
  if amount =0 then 1
else if amount <0 then 0
else if n=length coins then 0
else ((counter coins (amount - (List.nth coins n)) n)+(counter coins amount (n+1)))
in counter coins amount 0
;;


