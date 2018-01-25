(* problem 1*)
let rec fastexpt : int -> int -> int
= fun b n ->
  if n = 0 then 0
  else if n = 1 then b
  else if n mod 2 = 0 then fastexpt b (n/2) * fastexpt b (n/2)
  else b * fastexpt b (n-1);;

(* problem 2*)
let rec smallest_divisor_finder : int -> int -> int
= fun n d ->
  if n mod d = 0 then d
  else if d * d > n then n
  else smallest_divisor_finder n (d+1);;

let smallest_divisor : int -> int
= fun n ->
  if n mod 2 = 0 then 2
  else smallest_divisor_finder n 3;;

(* problem 3*)
let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) ->
  if n = 0 then fun b -> 0 
  else if n = 1 then f
  else fun a -> iter (n-1, f) (f a);;

(* problem 4*)
let rec product : (int -> int) -> int -> int -> int
= fun f a b ->
  if b = a then f b
  else (f b) * product f a (b-1);;

(* problem 5*)
let rec dfact : int -> int
= fun n ->
  if n < 2 then 1
  else n * dfact (n-2);;

(* problem 6*)
let rec drop : 'a list -> int -> 'a list
= fun l n ->
  if n = 0 then l
  else match l with
       |[] -> []
       |hd::tl -> drop tl (n-1);;

(* problem 7*)
let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst ->
  match lst with
  |[] -> ([], [])
  |hd::tl -> match hd with
            |(x,y) -> match unzip tl with
                      |(a,b) -> (x::a,y::b);;

(* problem 8*)
let rec change : int list -> int -> int
= fun coins amount ->
  if amount = 0 then 1
  else if amount < 0 then 0
  else match coins with
       |[] -> 0
       |hd::tl -> (change tl amount) + change coins (amount-hd);;
