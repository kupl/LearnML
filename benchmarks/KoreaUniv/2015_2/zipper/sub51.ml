let rec zipper : int list * int list -> int list
=fun (a,b) -> match a,b with
 |[],b -> b
 |a,[] -> a
 |h1::t1,h2::t2 -> if h1<h2 then h1::(zipper (t1, b))
              else h2::(zipper (a, t2));;

