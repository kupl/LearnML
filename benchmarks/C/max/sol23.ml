let rec max : int list -> int
=fun l -> match l with
| [] -> failwith "Null"
| [hd] -> hd
| hd::tl -> if hd > max tl then hd else max tl;;
 