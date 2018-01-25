let rec zipper : int list * int list -> int list 
= fun (l1,l2) ->
match (l1,l2) with
| ([],[]) -> []
| (l1,[]) -> l1
| ([],l2) -> l2
| (h1::t1,h2::t2) -> if h1>h2 then h2::zipper (l1,t2) else h1::zipper (t1,l2);;
