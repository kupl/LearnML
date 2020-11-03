type btree=
|Empty
|Node of int*btree*btree

let rec mem:int->btree->bool
=fun n tree-> match tree with
|Node (a,left,right) ->if a=n then true
else if (mem n left)||(mem n right) then true
else false
|Empty->false ;;