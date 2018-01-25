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