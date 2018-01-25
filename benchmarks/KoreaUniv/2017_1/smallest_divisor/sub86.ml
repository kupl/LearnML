(*problem2*)
let smallest_divisor :int -> int = fun n->
if (n<0) then raise (Failure "error: n is smaller than 0")
else if (n=0) then 2
else let a=n in
let rec f a i =
if (i*i) > a then a
else if (a mod i) = 0 then i
else f a (i+1) in f a 2;;