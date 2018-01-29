(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin
= fun b1 b2 -> let rec frev x rev = match x with 
                                    |[] -> rev 
                                    |h::t -> frev t (h::rev) in
               let rec bsum l1 l2 = match l1 with
                                    |[] -> [ZERO](**)
                                    |h1::t1 -> match l2 with
                                               |[] -> [ZERO](**)
                                               |h2::t2 -> if (h1 == ZERO) && (h2 == ZERO) then if t1 == [] && t2 == [] then [ZERO] else
                                                                                               if t1 == [] then ZERO::t2 else
                                                                                               if t2 == [] then ZERO::t1 else
                                                                                               ZERO::(bsum t1 t2) else
                                                          if (h1 == ZERO && h2 == ONE) || (h1 == ONE && h2 == ZERO) then if t1 == [] && t2 == [] then [ONE] else
                                                                                                                         if t1 == [] then ONE::t2 else
                                                                                                                         if t2 == [] then ONE::t1 else
                                                                                                                         ONE::(bsum t1 t2) else
                                                          if t1 == [] && t2 == [] then [ZERO; ONE] else
                                                          if t1 == [] then ZERO::(bsum [ONE] t2) else
                                                          if t2 == [] then ZERO::(bsum [ONE] t1) else
                                                          ZERO::(bsum [ONE] (bsum t1 t2)) in
	       let rec f result l1 l2 = match l1 with
                                        |[] -> [ZERO](**)
                                        |h1::t1 -> if h1 == ZERO then if t1 != [] then f result t1 (ZERO::l2) else
                                                                      result else
                                                   if h1 == ONE then if t1 != [] then f (bsum result l2) t1 (ZERO::l2) else
                                                                     bsum result l2 else [ZERO](**) in 
               let b1rev = frev b1 [] in
               let b2rev = frev b2 [] in
               frev (f [ZERO] b1rev b2rev) []
