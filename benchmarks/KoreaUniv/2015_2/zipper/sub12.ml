let rec zipper : int list * int list -> int list
=fun (a,b) -> match a with 
[]-> (match b with []->[]
|h::t->(zipper(b,a)))	 
|h::t->[h]@(zipper(b,t));;
