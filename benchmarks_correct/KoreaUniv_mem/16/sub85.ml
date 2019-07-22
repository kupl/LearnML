type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree ->
match tree with 
|Empty->false
|Node(x,y,z)->
if(x=n) then true 
else if (mem n y) then true 
else if (mem n z) then true 
else false;;
