let rec filter pred lst =
match lst with
|[] -> []
|hd::tl -> if (pred hd) then hd::(filter pred tl) 
else (filter pred tl);;

let rec zipper lst1 lst2 =
match lst1 with
|[] -> lst2
|hd::tl -> (match lst2 with
|[] -> lst1
|hd2::tl2 -> hd::hd2::(zipper tl tl2));;

let rec iter (n,f) =
match n with
|0 -> (function x -> x)
|1 -> f
|_ -> function x -> f ((iter(n-1, f)) x);;

type exp = X
|INT of int
|ADD of exp * exp
|SUB of exp * exp
|MUL of exp * exp
|DIV of exp * exp
|SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun e -> match e with
|INT e -> e
|ADD (e, e2) -> (calculator e) + (calculator e2)
|SUB (e, e2) -> (calculator e) - (calculator e2)
|MUL (e, e2) -> (calculator e) * (calculator e2)
|DIV (e, e2) -> (calculator e) / (calculator e2)
|SIGMA (e, e2, e3) -> 375;;

