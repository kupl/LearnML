
let rec product : (int -> int) -> int -> int -> int
= fun f a b -> let x = a in
				if x < b then (f x) * (product f (x+1) b)
				else f b
