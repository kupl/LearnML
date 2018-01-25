let rec zipper : int list * int list -> int list
=fun (a,b) -> match a with
[] -> b |
ha::ta -> ha::match b with
  [] -> zipper (ta,[]) |
  hb::tb -> hb:: zipper (ta, tb)
