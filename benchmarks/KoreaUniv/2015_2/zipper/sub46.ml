let rec zipper : int list * int list -> int list
=fun (a,b) -> match (a,b) with
							|([],[]) -> []
							|([],b) -> b
							|(a,[]) -> a
							|(hda::tla, hdb::tlb) -> hda::(hdb::zipper(tla, tlb));;

