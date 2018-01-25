let rec zipper lst1 lst2 =
match lst1 with
|[] -> lst2
|hd::tl -> (match lst2 with
|[] -> lst1
|hd2::tl2 -> hd::hd2::(zipper tl tl2));;
