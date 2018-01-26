(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec binexp n = if n>0 then 2*binexp(n-1) else 1;;

let rec bintodec : bin->int
= fun n -> match n with|[]->0|hd::tl->if hd=ONE
then (binexp (length n-1))+(bintodec tl) else (bintodec tl);;

let rec dectobin : int->bin
= fun n -> if n>0 then (if (n mod 2) = 1
	then (dectobin (n/2))@[ONE] else (dectobin (n/2))@[ZERO])
else [];;

let bmul : bin -> bin -> bin
= fun b1 b2 -> let x=((bintodec b1)*(bintodec b2)) in
if x=0 then [ZERO] else dectobin x;;





