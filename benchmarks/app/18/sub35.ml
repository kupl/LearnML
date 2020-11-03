let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> match l1 with
  |h :: t -> h :: app t l2
  |[] -> l2
;;
