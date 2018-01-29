(*problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec lislen : bin -> int
= fun a -> match a with
			|[] -> 0
			|hd::tl -> 1+(lislen tl)

let rec pow : (int * int) -> int
= fun (a, b) -> match b with
				|0 -> 1
				|1 -> a
				|i -> pow(a,(i-1))*a

let rec biToDec : bin -> int
= fun a ->match a with
			|[] -> 0
			|hd::tl -> if hd = ZERO then (biToDec tl)
						else if hd = ONE then pow(2,(lislen tl))+(biToDec tl)
						else raise (Failure "input error")

let rec decToBi : int -> bin
= fun n ->if n = 0 then [ZERO]
			else if n = 1 then [ONE]
			else if n mod 2 = 0 then decToBi(n/2)@[ZERO]
			else if n mod 2 = 1 then decToBi(n/2)@[ONE]
			else raise (Failure "input error")

let rec bmul : bin -> bin -> bin
= fun b1 b2 -> let x = (biToDec b1) * (biToDec b2) in (decToBi x)