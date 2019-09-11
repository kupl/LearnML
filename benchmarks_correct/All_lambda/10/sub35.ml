type lambda = V of var
	   | P of var * lambda
	   | C of lambda * lambda

and var = string


let add_element x set =
	x::set ;;

let rec check x set =
	if (List.mem x set) then true else false;;



let rec sub_check lambda set=
	match lambda with
	P (x, m) -> (sub_check m (add_element x set))
	|V n -> (check n set)
	|C (m1, m2) -> (sub_check m1 set) && (sub_check m2 set) ;;
	

let rec check lambda =
	sub_check lambda []
