let rec del : 'a list -> 'a -> 'a list
= fun lst x -> match lst with
| [] -> []
| hd::tl -> if hd = x then (del tl x) else [hd]@(del tl x);;

let rec uniq : 'a list -> 'a list
= fun lst -> match lst with
| [] -> []
| hd::tl -> [hd]@(uniq (del tl hd));;

let app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> uniq (l2@l1);;

