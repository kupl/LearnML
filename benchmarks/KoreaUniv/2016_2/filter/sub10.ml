let rec filter: ('a -> bool) -> 'a list -> 'a list
= fun f l -> match l with
| [] -> []
| h :: t -> if f h then h :: (filter f t) else filter f t;;
