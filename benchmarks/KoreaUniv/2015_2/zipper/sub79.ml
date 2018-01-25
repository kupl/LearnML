let rec zipper : int list * int list -> int list
=fun (a,b) ->
match a with
[]-> b
|[x]-> x::b
|h::t-> h::(zipper (b,t))
