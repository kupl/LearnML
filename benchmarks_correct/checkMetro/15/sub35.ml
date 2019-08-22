type var = string
type lambda = V of var
              | P of var * lambda
							| C of lambda * lambda

let rec isContain (st : var) (l : var list) : bool = 
	match l with
	| [] -> false
	| hd::tl -> (if st = hd then true else (isContain st tl))            
		


let rec subCheckMetro (m : lambda) (l : var list) : bool =
	match m with
	| V n -> isContain n l
	| P (nm, mtr) -> subCheckMetro mtr (List.append [nm] l)
	| C (m1, m2) -> (subCheckMetro m1 l) && (subCheckMetro m2 l) 



let check (m : lambda) : bool =
	subCheckMetro m []
	
