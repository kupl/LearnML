let rec zipper ( list1 , list2 ) =
	match list1 with
		[]->list2
		|hd_of_l1::tail_of_l1 -> match list2 with
									[]->list1
									|hd_of_l2::tail_of_l2 -> hd_of_l1::(hd_of_l2::(zipper (tail_of_l1 , tail_of_l2)))
	