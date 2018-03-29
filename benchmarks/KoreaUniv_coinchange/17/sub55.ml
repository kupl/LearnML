let rec insert a l = match l with
					|[] -> [a]
					|hd::tl -> if a > hd then a::hd::tl else hd::insert a tl

let rec rev_sort l = match l with
					|[]->[]
					|hd::tl -> insert hd (rev_sort tl)

let rec change : int list -> int -> int
= fun  coins amount -> let rs_coins = rev_sort coins in
					if amount < 0 then 0 else match rs_coins with
						|[] -> 0
						|[a] -> if amount mod a = 0 then 1 else 0
						|hd::tl-> if hd > amount then change tl amount 
									else change rs_coins (amount-hd) + change tl amount
									
									