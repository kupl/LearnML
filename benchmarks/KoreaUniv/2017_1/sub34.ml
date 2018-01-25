let rec fastexpt : int -> int -> int
= fun b n ->
if n < 0 then raise (Failure "n is negative value")
else if n = 0 then 1
else if n = 1 then b
else if n mod 2 = 0 then fastexpt (b*b) (n/2)
else b * fastexpt b (n-1)
 
let rec smallest_divisor : int -> int
= fun n ->
 
let rec get_divisor : int -> int -> int =
fun n d ->
if n mod d = 0 then d
else if d * d < n then get_divisor n (d+2)
else n
 
in
 
if n mod 2 = 0 then 2
else get_divisor n 3
 
let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) ->
 
let id_func x = x
 
in
 
let compose f g x = f (g x)
 
in
 
let rec iter_fun n f =
if n = 0 then id_func
else compose f (iter_fun (n-1) f)
 
in
 
if n = 0 then id_func
else if n < 0 then raise (Failure "n is negative value")
else iter_fun n f
 
let rec product : (int -> int) -> int -> int -> int
= fun f a b ->
 
if a < b then f(a) * product f (a+1) b
else if f(a) = b then f(a)
else if a = b then f(a)
else raise (Failure "a > b")
 
let dfact : int -> int
= fun n ->
 
let even n = product (fun x -> 2*x) 1 (n/2)
 
in
 
let odd n = product (fun x -> 2*x-1) 1 ((n+1)/2)
 
in
 
if n < 0 then raise (Failure "n is negative value")
else if n mod 2 = 0 then even n
else odd n
 
let rec drop : 'a list -> int -> 'a list
= fun l n ->
match l with
| [] -> []
| hd :: tl -> if n = 0 then hd::tl else drop tl (n-1)
 
let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst ->
 
let get_x tl = match tl with
| (x,y) -> x
 
in
 
let get_y tl = match tl with
| (x,y) -> y
 
in
 
match lst with
| [] -> ([], [])
| (x,y)::tl -> ([x]@get_x(unzip(tl)), [y]@get_y(unzip(tl)))
 
let rec change : int list -> int -> int
= fun coins amount ->
 
let rec coin_reduce lst v =
match lst with
| [] -> 0
| hd::tl -> if (v-hd) = 0 then 1 else if (v-hd) < 0 then 0 else coin_reduce tl v + coin_reduce lst (v-hd)
 
in
 
if amount = 0 then(if coins = [] then 0 else 1)
else if amount < 0 then 0
else coin_reduce coins amount