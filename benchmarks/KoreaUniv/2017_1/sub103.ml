(* 1 *)
let rec fastexpt b n =
if n = 0 then 1
else
(if n mod 2 = 0 then (fastexpt b (n/2))*(fastexpt b (n/2))
else b * (fastexpt b (n-1))) ;;

(* 2 *)
let smallest_divisor n =
    if n mod 2 = 0 then 2
    else begin
        let squareRootOfn = sqrt (float_of_int n) in
        let intSquareRootOfn = int_of_float (floor squareRootOfn) in
        let rec check n i =
            if i <= intSquareRootOfn then (
            if n mod i = 0 then i
            else check n (i+2)
            )
            else n in
        check n 3
    end;;

(* 3 *)
let rec iter(n,f) x: int
   =  if n = 0 then x
   else f (iter((n-1),f) x);;

(* 4 *)
let rec product f a b : int
= if a = b then f a
  else a*(product f (a+1) b);;

(* 5 *)
let rec dfact n : int
= if n mod 2 =0
  then (
   if n = 2 then 2
   else n*(dfact (n-2))
  )
  else (
   if n = 1 then 1
   else n*(dfact(n-2))
  );;

(* 6 *)
let rec  drop l n =
  if n>0 then
  (match l with
   [] -> []
   |hd::tl -> drop tl (n-1)
  )
  else l;;

(* 7 *)
let rec unzip l=
match l with
   [] -> ([], [])
  |(x,y)::tl ->
    let tuple = unzip tl in
    ((x::(fst tuple)), (y::(snd tuple)));;

(* 8 *)
let rec change2 l amt =
  let rev_list = List.rev l in
  if amt<0 then 0
  else if amt = 0 then 1
  else(
   match rev_list with
   [] -> 0
  |hd::tl -> (change2 tl amt) + (change2 l (amt-hd)));;
