
(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec binary_to_int : bin -> int -> int
= fun b n -> match b with
           | hd::tl -> if hd=ONE then binary_to_int tl (2*n+2) else if hd=ZERO then binary_to_int tl (2*n) else n/2
           | _ -> n/2

let rec int_to_binary : int -> bin
= fun n -> match n with
           | 0 -> []
           | _ -> if n mod 2=1 then ONE::(int_to_binary (n/2)) else if n mod 2=0 then ZERO::(int_to_binary (n/2)) else []
let rec reverse_binary : bin -> bin -> bin
= fun b1 b2 -> match b1 with
           | hd::tl -> reverse_binary tl (hd::b2)
           | _ -> b2
let bmul : bin -> bin -> bin
= fun b1 b2 -> if b1=[ZERO] then [ZERO] else if b2=[ZERO] then [ZERO]   else reverse_binary (int_to_binary((binary_to_int b1 0)*(binary_to_int b2 0))) []
