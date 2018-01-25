let rec zipper (l1, l2) =
match l1 with
[] -> l2
|h::t -> h::zipper(l2,t);;


