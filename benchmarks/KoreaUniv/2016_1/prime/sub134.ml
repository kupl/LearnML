 let rec is_prime ?(m=2) n =
    match n with 
	| 1 -> false | _ -> 
    m * m > n || n mod m <> 0 && is_prime ~m:(m+1) n;;
  
 	