(* problem 1*)
let rec fastexpt : int -> int -> int
= fun b n ->
if n=0 then 1
else if n mod 2 = 0 
then begin
(fastexpt b (n/2))*(fastexpt b (n/2)) end
 else b*(fastexpt b (n-1));;

(* problem 2*)
let smallest_divisor : int -> int
= fun n ->
for i=n to i=2 do
if n mod i = 0 then 
let divisor=i
done;;

(* problem 3*)
let iter : int * (int -> int) -> (int -> int)
= fun (n,f) ->
if n=0 then fun x->x

(* problem 4*)
let product : (int -> int) -> int -> int -> int
= fun f a b -> 

(* problem 5*)
let dfact : int -> int
= fun n ->
 if n mod 2 = 0 then

(* problem 6*)
let drop : 'a list -> int -> 'a list
= fun l n ->

(* problem 7*)
let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> 

(* problem 8*)
let change : int list -> int -> int
= fun coins amount ->