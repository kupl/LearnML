let isnil l 
	= match l with
		|[] -> true
		|_ -> false;;

let rec reverse_list lst 
	= match lst with
		| [] -> []
		| h::t -> (reverse_list t)@[h];;

let rec loop coins amount sum n 
	= match coins with
		| h::t -> if (n = -2) then (loop coins amount 0 (amount/h)) else (if(isnil t) 
			then (if (sum > amount )
				then 0 
				else 1) 
			else (if (n = -1)
				then 0
				else (loop coins amount sum (n-1))+(loop t amount (sum+(n*h)) (amount/h))));;

let change : int list -> int -> int
= fun coins amount -> if (amount>0) then (loop (reverse_list coins) amount 0 -2) else (if (amount = 0) then 1 else 0);;




