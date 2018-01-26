(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin
= fun b1 b2 -> if (b1 = [ZERO] || b2 = [ZERO]) then [ZERO]
else let rec bintodec b e = match b 
with hd::tl -> (match hd
with ZERO -> 0 * e + (bintodec tl e/2)
| ONE -> 1 * e + (bintodec tl e/2))
| [] -> 0
in let rec dectobin d =
if ( d = 0 ) then []
else (dectobin (d/2))@(if d mod 2 = 0 then [ZERO] else [ONE])
in dectobin ( (bintodec b1 (1 lsl ((List.length b1)-1)))*(bintodec b2 (1 lsl ((List.length b2)-1))) )
