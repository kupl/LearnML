let rec pascal : int * int -> int
=fun (x,y) ->
if x<y then 0
else if y=0 then 1
else if x=y then 1
else pascal ((x-1),(y-1))+pascal((x-1),(y));;

