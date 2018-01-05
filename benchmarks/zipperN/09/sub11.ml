let rec zipperN e=
   match e with 
     | []->[]  
     | he1 ::he2::ta1 -> 
 	let rec zipper f=
          match f with 
           |[], [] ->  []
           |h::t,[] -> h::t
           |[],h::t -> h::t
           |h1::t1,h2::t2 -> h1+0::h2+0::zipper(t1,t2)
	    in
          if ta1 = [] then 
   			zipper(he1,he2) 
 
            else        
			zipperN(zipper(he1,he2)::ta1) 

            