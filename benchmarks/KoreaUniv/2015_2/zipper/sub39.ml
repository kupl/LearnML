let rec zipper : int list * int list -> int list
=fun (a,b) ->
match a, b with
|(_,[]) -> a
|([],_) -> b
|(ah::at, [y]) -> ah::y::at
|(ah::at, bh::bt)-> ah::bh::zipper(at, bt);; 
