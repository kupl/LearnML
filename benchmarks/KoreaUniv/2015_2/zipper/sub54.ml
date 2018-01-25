let rec zipper : int list * int list -> int list
=fun (a,b) ->
match a,b with
|[],b -> b
|a,[]->a
|ha::ta, hb::tb -> ha::hb::zipper(ta,tb)
