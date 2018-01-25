let rec zipper (lst1,lst2) =
 match lst1 with
  |[] -> lst2
  |[hd] -> hd::lst2
  |hd::tl -> hd::(zipper (lst2,tl)) 
  