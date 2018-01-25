(* Problem 1 *)
let rec fib a =
if a == 0 then 1
else if a == 1 then 1
else fib (a-1) + fib (a-2);;

(* Problem 2 *)
let rec pascal pair =
let first = match pair with (x,_) -> x in
let second = match pair with (_,y) -> y in
if second == 0 then 1
else if second == first then 1
else pascal (first-1, second-1) + pascal (first-1, second);;

(* Problem 3 *)
let prime a =
if a == 1 then true
else
let i = ref 2 in
while a mod !i != 0 do
i := !i + 1
done;
if !i == a then true
else false;;


(* Problem 4 *)
let sigma f a b =
let result = ref 0 in
for i = a to b do
result := !result + f i
done;
!result;;
