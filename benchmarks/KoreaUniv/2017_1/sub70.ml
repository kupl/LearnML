(* problem 1*)
let fastexpt : int -> int -> int
= fun b n-> 
let rec exp b n =
if n = 0 then 1
else if n mod 2 = 1 then b * (exp b (n-1))
else let a = (exp b (n/2)) in a*a  in exp b n;; 
(*problem 2*)
let smallest_divisor : int -> int
= fun n ->
let rec findfactor n factor = 
if factor * factor > n then n
else if n mod factor = 0 then factor
else findfactor n (factor+1) in findfactor n 2;;
(*problem 3*)
let iter : int * (int -> int) -> (int -> int)
= fun (n,f) ->
let rec ite n f =
if n = 0 then fun x -> x
else fun x -> ite (n-1) f (f x) in ite n f;;
(*problem 4*)
let product : (int -> int) -> int -> int -> int
= fun f a b -> 
let rec prod f a b =
if b > a then f(f b) * prod f a (b-1)
else f(f a) in prod f a b;;
(*problem 5*)
let dfact : int -> int
= fun n ->
let rec dfac n =
if n < 2 then 1
else n * dfac (n-2) in dfac n;;
(*problem 6*)
let drop : 'a list -> int -> 'a list
= fun l n ->
let rec dro l n = 
if n = 0 then l
else
match l with
| [] -> []
| hd :: tl -> dro tl (n-1) in dro l n;;
(*problem 7*)
let unzip : ('a *'b) list -> 'a list * 'b list
= fun lst ->
let rec unzi lst =
match lst with
| [] -> ([], []) 
| (a,b)::tl ->
let (fst,snd) = unzi tl in ((a::fst), (b::snd)) in unzi lst;;
(*problem 8*)
let change : int list -> int ->int
= fun coins amount -> 
let rec chang coins amount =
if amount = 0 then 1
else if amount < 0 then 0
else 
match coins with 
| [] -> 0
| hd::tl -> 
(chang tl amount) + (chang coins (amount-hd)) in chang coins amount;;
