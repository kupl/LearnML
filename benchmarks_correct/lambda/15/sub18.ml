type lambda = V of var 
	| P of var * lambda
    | C of lambda * lambda
and var = string

let check lambda =
	let rec check met str_list=
	match met with
	| V a -> List.mem a str_list
	| P (a,V b) -> (a=b)||(List.mem b str_list)
	| P (a,b) -> check b (str_list@[a])
	| C (a,b) -> (check a str_list)&&(check b str_list)
	in
	check lambda []

