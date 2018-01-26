(*problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec bintoint : bin -> int -> int
= fun b a -> match b with
	[] -> a
	|[ZERO] -> a*2
	|[ONE] -> 2*a+1
	|hd::tl -> (match hd with |ZERO -> (bintoint tl (a*2)) |ONE -> (bintoint tl (a*2+1)));;

let rec inttobin : int -> bin -> bin
= fun b lst-> match b with
	|0 -> ZERO::lst
	|1 -> ONE::lst
	|_ -> if (b mod 2)=0 then (inttobin (b/2) (ZERO::lst)) else (inttobin ((b-1)/2) (ONE::lst));;

let bmul : bin -> bin -> bin
= fun b1 b2 -> inttobin ((bintoint b1 0) * (bintoint b2 0)) [];;
