let rec zipper ((l1:int list), (l2:int list)) = 
	match l1 with 
	[] -> l2 
	|hd::tl -> hd::zipper (l2, tl);; 