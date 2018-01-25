let rec fib : int -> int
= fun n ->
if n=0 then 0
else if n= 1 then 1
else fib(n-1) + fib(n-2)

let rec pascal : int * int -> int
=fun(n1,n2)->
if n2 = 0 then 1
else if n1 = n2 then 1
else pascal(n1-1,n2-1)+pascal(n1-1,n2)

let rec prime : int ->  bool
=fun n ->
if n=1 then false
else modd n 2
and modd n k =
if n = k then true
else if n mod k = 0 then false
else modd n (k+1)

let rec sigma : (int -> int) -> int -> int -> int
= fun f a b ->
if a<b then (sigma f a (b-1)) + (f b)
else f b
