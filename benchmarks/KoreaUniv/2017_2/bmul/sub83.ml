(*problem 7*)
type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin
= fun b1 b2 ->
let rec d2b b =
if b = 0 then [] 
else  
if (b mod 2 = 0)
then ZERO :: d2b(b/2)
else ONE :: d2b(b/2)
in let rec b2i b
= match b with
| [] -> 0
| hd::tl -> 
match hd with
| ONE -> (1 lsl List.length tl) + b2i tl 
| ZERO -> b2i tl in List.rev (d2b ((b2i b1) * (b2i b2)));;
