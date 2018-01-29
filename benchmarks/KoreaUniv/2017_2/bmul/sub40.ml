(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec bmul : bin -> bin -> bin
= fun b1 b2 -> let rec rev l = match l with
|hd::tl -> (rev tl)@[hd]
|_ -> [] in let rec bindec b1 = match b1 with
|hd::tl -> (match hd with
  |ZERO -> 0 + (bindec tl) * 2
  |ONE -> 1 + (bindec tl) * 2 )
|_ -> 0 in let rec decbin d1 = if(d1/2) = 0 then [ONE] else
(match (d1 mod 2) with
 |1 -> (decbin (d1/2))@[ONE]
 |0 -> (decbin (d1/2))@[ZERO]
 |_->[ZERO]) in decbin(bindec(rev(b1)) * bindec(rev(b2)))
