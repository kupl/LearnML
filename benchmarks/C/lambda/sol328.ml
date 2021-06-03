type lambda = V of var
			|P of var * lambda
			|C of lambda * lambda
and var = string

let rec findStringinList ((l : string list),(var : string)) : bool =
	match l with
		[] -> false
		|hd::tl -> if((hd = var)) then true else findStringinList(tl,var)

let rec checkwithList ((m : lambda),(l : string list)) : bool =
	match m with
				V(var_) -> if(findStringinList(l,var_)) then true else false
				|P(var_,lambda_) -> checkwithList(lambda_ , var_::l)
				|C(lambda_1,lambda_2) -> checkwithList(lambda_1,l)&&checkwithList(lambda_2,l)

let check (m : lambda) : bool =
		 match m with
		 V(var_) -> false
		|P(var_,lambda_) -> checkwithList(lambda_,[var_])
		|C(lambda_1,lambda_2) -> checkwithList(lambda_1,[])&&checkwithList(lambda_2,[])

