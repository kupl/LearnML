(* Problem 1 *)
let rec fib n=
if n=0 then 0
else if n=1 then 1
else fib (n-1) + fib (n-2)

(* Problem 2 *)
let rec fac n m =
if n<m then fac n (m-1) * m
else n

let rec makit (a, b) =
if b=0 then 1
else if b=1 then a
else fac (a-b+1) a / fac 1 b

let rec pascal (n1, n2) =
if n1-n2<n2 then makit (n1, n1-n2)
else makit (n1, n2)

(* Problem 3 *)
let rec isprim (n, cont) =
if cont=1 then true
else if n mod cont = 0 then false
else isprim (n, (cont-1))

let rec prime n =
if n<2 then false
else if n=2 then true
else if n=3 then true
else if n mod 2 = 0 then false
else if n mod 3 = 0 then false
else isprim (n, int_of_float (sqrt (float_of_int n)))

(* Problem 4 *)
let rec sigma f a b =
if b > a then sigma f a (b-1) + f (b)
else f (a)