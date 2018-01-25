let rec fastexpt : int -> int -> int
= fun b n -> if n = 0 then 1
			else if n = 1 then b
			else if n mod 2 = 0 then (fastexpt b (n/2))*(fastexpt b (n/2))
			else b*(fastexpt b (n-1))



let smallest_divisor : int -> int
= fun n -> let rec divisor a b 
				= if a mod b = 0 then b else (divisor a (b+1)) in
			let sq x = x*x in
			let x = 2 in if n > (sq x) then divisor n x
			else n

let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> if n = 0 then fun x -> x
				else if n = 1 then f
				else fun x -> iter((n-1),f) (f x)

let rec product : (int -> int) -> int -> int -> int
= fun f a b -> let x = a in
				if x < b then (f x) * (product f (x+1) b)
				else f b


let rec dfact : int -> int
= fun n -> let double : int -> int = fun x -> (x+x) in
			let double_m_1 : int -> int = fun x -> (x+x-1) in
			if n = 1 then 1
			else if n = 2 then 2
			else if n mod 2 = 0 then (product double 1 (n/2))
			else if n mod 2 = 1 then (product double_m_1 1 ((n+1)/2))
			else raise(Failure "TYPE ERROR")

let rec drop : 'a list -> int -> 'a list
= fun l n -> match l with
			|[]->[]
			|hd::tl -> if n = 0 then hd::tl else drop tl (n-1)

let rec unzip : ('a * 'b) list -> 'a list * ' b list
= fun lst -> let fst p = match p with (x,_) -> x in
			let sec p = match p with (_,x) -> x in
			match lst with
			|[]->([],[])
			|[(x,y)] -> ([x],[y])
			|hd::tl -> (fst hd::fst (unzip tl),sec hd::sec (unzip tl))

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
									
									