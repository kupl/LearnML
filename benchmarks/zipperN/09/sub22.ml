let rec zipperN (lol : int list list) = let rec tail ll = match ll with
  			[] -> []
  			|(h1::t1) -> if h1 = [] then tail t1
  				     else (List.tl h1 :: tail t1) in
			let rec zipper lll = match lll with
  			[] -> []
  			|(h1::t1) -> match h1 with 
  				     [] -> zipper t1  
                                     |(h2 :: t2) -> (h2 :: zipper t1) in
  			match lol with  
  			|[] -> [] |(h1::t1) -> (zipper lol) @ (zipperN (tail lol));;