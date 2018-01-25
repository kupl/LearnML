let rec zipper : int list * int list -> int list
=fun (l1, l2) ->
  match l1 with
  |[] -> []
  |hd::tl -> if tl = [] then hd::l2 else hd::(zipper (l2, tl))
