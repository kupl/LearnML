let rec zipper : int list * int list -> int list =fun (a,b) -> 
match a,b with
|[],_ -> b
|_,[] -> a
|hda::tla, hdb:: tlb -> if hda<hdb then hda :: zipper(tla,b) else hdb::zipper(a,tlb);; 