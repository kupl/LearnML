type lambda = V of var
	|P of var * lambda
	|C of lambda * lambda
and var = string

let rec contains listOfArea s =
	match listOfArea with
	|[] -> false
	|h::t -> (if h = s then true
		else contains t s)

let rec check2 x listOfArea =
	match x with
	|V s -> contains listOfArea s
	|P (n, m) -> check2 m (n::listOfArea)
	|C (m1, m2) -> check2 m1 listOfArea && check2 m2 listOfArea

let check x =
	match x with
	|V s-> true
	|_ -> check2 x []