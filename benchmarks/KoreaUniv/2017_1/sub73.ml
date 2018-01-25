exception NO_NEGATIVE_INTEGERS
exception ANSWER_TO_BE_1
(* problem 1*)
let rec fastexpt : int -> int -> int
= fun b n -> 
if n == 0 then 1
else if n<0 then raise (NO_NEGATIVE_INTEGERS)
else if (n mod 2) == 0 then fastexpt b (n/2) * fastexpt b (n/2)
else b * fastexpt b (n-1)

(* problem 2*)

let smallest_divisor : int -> int
= fun n -> 
if n<0 then raise NO_NEGATIVE_INTEGERS
else if n mod 2 == 0 then 2
else
  let rec loop n d =
    if (d*d)>n then n
    else
      if n mod d == 0 then d
      else loop n (d+2) in
        loop n 3

(* problem 3*)

let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> 
let identity x = (x+0) in
  let compose f g x = f(g x) in
    if n == 0 then identity
    else if n<0 then raise (NO_NEGATIVE_INTEGERS)
    else
      compose f (iter(n-1,f))

(* problem 4*)

let rec product : (int -> int) -> int -> int -> int
= fun f a b -> 
if a > b then raise ANSWER_TO_BE_1
else
  let rec loop f a n =
    if n == b then f b
    else
      f n * loop f a (n+1) in loop f a a

(* problem 5*)

let dfact : int -> int
= fun n -> 
if n<0 then raise (NO_NEGATIVE_INTEGERS)
else if n == 0 then raise ANSWER_TO_BE_1
else
  if n mod 2 == 0 then
    let rec even n d =
      if n == d then n
      else d * even n (d+2) in even n 2
  else
    let rec odd n k =
      if n == k then n
      else k * odd n (k+2) in odd n 1

(* problem 6*)

let rec drop : 'a list -> int -> 'a list
= fun l n -> 
if n == 0 then l
else if n<0 then raise NO_NEGATIVE_INTEGERS
else
  match l with
  |[]->[]
  |hd::tl -> drop tl (n-1);;

(* problem 7*)

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> 
match lst with
|[]->([],[])
|(a,b)::tl->
let first(a,b) = a in
  let second(a,b) = b in
    (first(a,b)::first(unzip tl), second(a,b)::second(unzip tl))

(* problem 8*)

let rec change : int list -> int -> int
= fun coins amount -> 
if amount == 0 then 1
else if amount<0 then 0
else
  match coins with
  |[]->0
  |hd::tl -> change tl amount + change (hd::tl) (amount-hd)

