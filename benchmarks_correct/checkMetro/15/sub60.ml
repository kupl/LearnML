type var = string;;

type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda;;

let rec contains = fun (l, item)->
	match l with
	|[] -> false 
	|h :: tail ->
		if(item = h) then true
		else contains(tail, item);;

let rec lambda = fun (l, m)->
	match m with
	|V n -> contains(l, n)
	|P (id, area) -> lambda(id::l, area) 
	|C (m1, m2) -> lambda(l,m1) && lambda(l,m2);;

let check = fun m -> lambda ([], m);;

