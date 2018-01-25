(* problem 7*)

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst ->
   match lst with
     [] -> ([], []) (*when lst is empty, just return a tuple with 2 empty lists as elements*)
     |(hd1, hd2)::tl ->                      
     		     let (tl1, tl2) = unzip tl in (hd1::tl1, hd2::tl2);; 
	(*When lst is not empty, it means it has a head and a tail. And the head is a tuple in this case, so expressed like (hd1, hd2). And we have to return a tuple with 2 lists as elements, so it is expressed as (tl1, tl2), and just call the function "unzip" recursively until the tl becomes an empty list.*)