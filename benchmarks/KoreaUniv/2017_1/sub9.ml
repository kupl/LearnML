(* problem 1*)

let rec fastexpt : int -> int -> int
= fun b n ->
if n<0||b<=0 then raise(Invalid_argument "fastexpt")
else if n=0 then 1
else if (n mod 2) = 0 then (fastexpt b (n/2))*(fastexpt b (n/2))
else b*(fastexpt b (n-1))

(* problem 2*)

let smallest_divisor : int -> int
= fun n ->
let rec div : int -> int -> int
= fun n cnt ->
if (n mod cnt) = 0 then cnt else div n (cnt+1) in div n 2

(* problem 3*)

let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) ->
if n=0 then (fun x -> x) 
else fun x -> iter (n-1,f) (f x)

(* problem 4*)

let rec product : (int -> int) -> int -> int -> int
= fun f a b ->
if a=b then f a
else if a < b then (f a)*(product f (a+1) b) else raise (Invalid_argument "product") 

(* problem 5*)

let rec dfact : int -> int
= fun n ->
if n<0 then raise (Invalid_argument "dfact")
else if n=1||n=0 then 1
else n*(dfact (n-2))

(* problem 6*)

let rec drop : 'a list -> int -> 'a list
= fun l n ->
match l with
| [] -> []
|hd::tl -> 
if n=0 then hd::tl else drop tl (n-1)


(* problem 7*)

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> 
match lst with
| [] -> ([], [])
| (l1,l2)::tl ->
let a, b = unzip tl in l1::a, l2::b

(* problem 8*)

let change : int list -> int -> int
= fun coins amount ->
if amount<0 then 0 else
match (coins,amount) with
| ([], _) -> 0
| (_, 0) -> 1
| (hd::tl,_) ->
let cnt = Array.make (amount+1) 0 in cnt.(0) <-1;
List.iter (fun coin -> for i = coin to amount do cnt.(i) <- cnt.(i)+cnt.(i-coin) done) coins;
cnt.(amount) 
