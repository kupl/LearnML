(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin
= fun b1 b2 -> let rec length l = match l with | [] -> 0 | hd::tl -> 1 + length tl in let rec exp k = if k = 0 then 1 else 2 * exp (k-1) in 
			let rec funa k = match k with | [] -> 0 | hd::tl -> if hd = ONE then (exp ((length k) - 1)) + funa tl else funa tl in
				let rec fund k = match k with | [] -> 0 | hd::tl -> if hd = ONE then (exp ((length k) - 1)) + fund tl else fund tl in
					let funb k = k mod 2 in
					let rec func k lis = if k = 1 then ONE::lis else if (funb k) = 1 then let lis = ONE::lis in func (k/2) lis else let lis = ZERO::lis in func (k/2) lis in  
						func (funa b1 * fund b2) [];;



