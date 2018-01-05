let rec zipper ( ( l1 : int list ), ( l2 : int list ) ) =
	match ( l1, l2 ) with
	( h1::t1, h2::t2 ) -> h1::h2::zipper( t1, t2 ) |
	( h1::t1, [] ) -> h1::zipper( t1, [] ) |
	( [], h2::t2 ) -> h2::zipper( [], t2 ) |
	( [], [] ) -> [];;