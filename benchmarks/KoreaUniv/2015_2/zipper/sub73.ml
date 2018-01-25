let rec zipper : int list * int list -> int list
=fun (a,b) -> match a with
              | [] -> b
              | hda :: tla -> (match b with
                            | [] -> a
                            | hdb :: tlb -> [hda ; hdb] @ (zipper (tla , tlb)) )
