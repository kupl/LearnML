(*problem 7*)
 
type digit = ZERO | ONE
type bin = digit list
 
let bmul : bin -> bin -> bin
= fun b1 b2 -> 
let rec bi_to_dec bexp n =
match bexp with
| [] -> 0
| hd::[] -> if hd=ZERO then n*2+0 else n*2+1
| hd::tl -> if hd=ZERO then bi_to_dec tl (n*2) else bi_to_dec tl (n*2+1)
in
let rec dec_to_bi dexp b_list =
if dexp = 1 then [ONE]@b_list
else if dexp = 0 then [ZERO]@b_list
else if dexp mod 2 = 1 then dec_to_bi ((dexp-1)/2) [ONE]@b_list
else dec_to_bi (dexp/2) [ZERO]@b_list 
in
let ex1 = bi_to_dec b1 0 in let ex2 = bi_to_dec b2 0 in
let dec_result = ex1 * ex2 
in
dec_to_bi dec_result []