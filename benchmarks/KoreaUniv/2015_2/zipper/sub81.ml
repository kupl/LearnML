let rec zipper : int list * int list -> int list
=fun (a,b) -> match b with
|[]->a@b
|hb::tb->
(match a with
|[]->b
|ha::ta->ha::hb::(zipper (ta,tb)))				
