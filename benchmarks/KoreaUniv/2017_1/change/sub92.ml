(* problem 8-solve*)

let change : int list -> int -> int
= fun coins amount ->  if amount < 0 then 0 else 
 			let combinations = Array.make (amount + 1) 0 in
  			combinations.(0) <- 1;
  			List.iter (fun coin ->
   			 for i = coin to amount do
     			 combinations.(i) <- combinations.(i)+combinations.(i-coin)
    			done
  			) coins;
  		combinations.(amount)