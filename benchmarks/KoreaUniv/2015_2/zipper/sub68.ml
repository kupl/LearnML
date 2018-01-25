let rec zipper lst1 lst2= match lst1  with | []-> lst2 | hd:: tl -> match lst2 with | []-> hd::tl | hd1::tl1-> ( hd::hd1::(zipper tl tl1) );;
