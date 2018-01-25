(* problem 1-solve*)

let rec fastexpt : int -> int -> int
= fun b n -> if n = 0 then 1 else if n =1 then b else 
	    if (n mod 2 = 0) then (fastexpt b (n/2))*(fastexpt b (n/2)) else 
            b*(fastexpt b ((n-1)/2))*(fastexpt b ((n-1)/2))

(* problem 2-solve*)

 let smallest_divisor : int -> int
    =fun n -> 
	let sqrt n = int_of_float (sqrt (float_of_int n)) in
	let rec temp n i = if n=1 then 1 else if i = sqrt n then n else 
               if (n mod i == 0) then i else temp n (i + 1) in
    	temp n 2;;
	


(* problem 3----solve*)

let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) ->  if n = 0 then fun x->x else 
				(fun f g x -> f(g x)) f@@(iter(n-1,f))




(* problem 4-solve*)
let rec product : (int -> int) -> int -> int -> int
= fun f a b -> if b=a then f b else f a*product f (a+1) b

(* problem 5-solve*)

let rec dfact : int -> int
= fun n -> let rec product f a b = if b=a then f b else f a*product f (a+2) b in
		if n mod 2 = 0 then 
		product (fun x->x) 2 n else product (fun x->x) 1 n 



(* problem 6-solve*)
let rec drop : 'a list -> int -> 'a list
= fun l n -> match l with
		|[] -> []
		|hd::tl-> if n = 0 then l
			else drop tl (n-1)


(* problem 7-solve*)

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> match lst with
		|[]->([],[])
		|(a,b)::tl-> 
		let (tla,tlb) = unzip tl in
		((a::(tla)),(b::(tlb)))
		
		
		

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




