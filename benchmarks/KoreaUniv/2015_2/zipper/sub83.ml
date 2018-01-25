let rec zipper : int list * int list -> int list
=fun (a,b) -> match (a,b) with 
|([],[])->[]
|(a, [])->a
|([], b)->b
|(h1::t1, h2::t2) -> h1::h2::zipper(t1, t2);;
