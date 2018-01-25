let rec zipper : int list * int list -> int list
=fun (a,b) -> match a,b with
[],[]-> []
|[],b -> b
|a,[]-> a
|h1::a',h2::b' -> h1::h2::zipper(a',b')
					
