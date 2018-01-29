(*problem7*)

type digit = ZERO
|ONE

type bin = digit list

let rec length : bin -> int = fun b ->
match b with
|[] -> 0
|h::t -> 1 + (length t);;

let rec power : int -> int = fun n ->
if n = 0 then 1 else 2 * (power (n-1));;

let rec bd : bin -> int = fun b ->
match b with 
|[] -> 0
|[ONE] -> 1
|[ZERO] -> 0
|ONE::t -> (power ((length b)-1)) + (bd t)
|ZERO::t -> bd t;;

let rec db : int -> bin = fun n ->
if n=0 then [ZERO] else if n=1 then [ONE]
else if (n mod 2 = 1) then (db (n/2))@ [ONE]
else if (n mod 2 =1) then (db (n/2)) @ [ONE]
else (db (n/2)) @ [ZERO];;

let bmul : bin -> bin -> bin = fun b1 b2 ->
db((bd b1) * (bd b2));;
