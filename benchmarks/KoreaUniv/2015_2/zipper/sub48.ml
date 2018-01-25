let rec zipper (l1, l2) =
				match (l1, l2) with
				([],[]) -> [] |
				(_,[]) -> l1 |
				([],_) -> l2 |
				(h1::t1,h2::t2) -> [h1;h2] @ (zipper (t1,t2));;
