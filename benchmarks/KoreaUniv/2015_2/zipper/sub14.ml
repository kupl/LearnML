let rec zipper : int list * int list -> int list
=fun (a,b) ->
match a,b with 
| [],[] -> [] 
| [],_ -> b 
| _,[] -> a
| hda::tla, hdb::tlb -> hda::hdb::zipper(tla,tlb)