(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let carry = ZERO

let rec getXElement : bin -> int -> int -> digit
= fun b l x -> if l>=x then ZERO else match b with
					| [] -> ZERO
					| hd::tl -> if l=x+1 then hd else (getXElement tl (l-1) x)

let rec getLength : bin -> int
= fun b -> match b with
		| [] -> 0
		| hd::tl -> (getLength tl) + 1

let rec exten: int -> bin
= fun x -> if x=0 then [] else ZERO::(exten (x-1))

let rec add : bin -> bin -> (digit * bin)
= fun b1 b2 -> match b1, b2 with
		| [], [] -> (ZERO, [])
		| [], _::_ -> (ZERO, [])
		| _::_, [] -> (ZERO, [])
		| hd1::tl1, hd2::tl2 -> let res = (add tl1 tl2) in
						match res with
							|(x,y) -> if hd1=hd2 then
									(hd1, x::y)
								  else if x=ZERO then
										(ZERO, ONE::y)
									else (ONE, ZERO::y)

let addBin : bin -> bin -> int -> bin
= fun b1 b2 x -> let b1 = b1@(exten x) in
			let len1 = (getLength b1) in
			let len2 = (getLength b2) in
			if len1<len2 then
				let b1 = (exten (len2-len1))@b1 in
					let res = (add b1 b2) in
						match res with
							|(x,y) -> if x=ZERO then y else ONE::y
			else let b2 = (exten (len1-len2))@b2 in
					let res = (add b1 b2) in
						match res with
							|(x,y) -> if x=ZERO then y else ONE::y
		

let rec cmul : bin -> digit -> bin
= fun b x -> match b with
		| [] -> []
		| hd::tl -> if hd=ZERO then ZERO::(cmul tl x) else x ::(cmul tl x)

let rec smul : bin -> bin -> int -> bin
= fun b1 b2 x -> match b2 with
			| [] -> []
			| hd::tl -> let first = (cmul b1 hd) in
				    let second = (smul b1 tl (x-1)) in
					addBin first second x

let rec bmul : bin -> bin -> bin
= fun b1 b2 -> (smul b1 b2 ((getLength b2)-1))
