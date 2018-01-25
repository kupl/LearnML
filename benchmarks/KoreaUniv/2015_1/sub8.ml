let rec pascal : int*int -> int
=fun (x,y) ->
match (x,y) with
0,0 | _,0 -> 1
|x,y when x=y -> 1
|_ -> pascal (x-1, y-1) + pascal (x-1,y);;

