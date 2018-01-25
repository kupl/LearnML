let rec zipper a b = 
 match (a,b) with
([],[]) -> []
| (hd::tl,[]) -> hd::tl
| ([],hd::tl) -> hd::tl
| (hd1::tl1,hd2::tl2) -> hd1 :: (zipper (hd2::tl2) tl1);;
