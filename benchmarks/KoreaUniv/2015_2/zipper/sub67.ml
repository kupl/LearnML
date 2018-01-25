let rec zipper : int list * int list -> int list
=fun (a,b) -> match a with
[] -> b
|h::t -> h::(zipper (b, t));;
