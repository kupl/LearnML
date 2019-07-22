type btree =
	| Empty
	| Node of int * btree * btree

let rec mem n btree=
match btree with
| Empty -> false
| Node(element,left_btree,right_btree)->
if  element==n then true
else mem n left_btree || mem n right_btree ;;
