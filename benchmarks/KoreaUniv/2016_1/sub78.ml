(* Problem 1 *)
let rec fib n =
if n=0 then 0 else
if n=1 then 1 else
fib (n-1) + fib (n-2);;

(* Problem 2 *)
let rec pascal (a,b) =
if b=0 then 1 else
if a=b then 1 else
pascal (a-1,b-1) + pascal (a-1,b);;

(* Problem 3 *)
let rec plus a n =
if n <= 1 then false else
if n mod a <> 0 then plus (a+1) n else
if n<>a && n mod a = 0 then false
else true;;

(* Problem 4 *)
let rec sigma f a b =
if a = b then f b else f a + sigma f (a+1) b;;

