(* problem 7 *)
type digit = ZERO | ONE
type bin = digit list
let rec length l = match l with 
  | [] -> 0
  | hd::tl -> 1 + (length tl)
let rec fastexpt
= fun n -> match n with
  | 1 -> 1
  | _ -> 2*(fastexpt (n-1))
let rec btod
= fun b n-> match b with
  | [] -> 0
  | hd::tl -> begin match hd with
          | ZERO -> 0 + (btod tl (n-1))
          | ONE -> (fastexpt n) + (btod tl (n-1))
          end
let rec dtob
= fun n -> match n with
  | 0 -> [ZERO]
  | 1 -> [ONE]
  | _ -> if (n mod 2 = 1) then (dtob (n/2))@[ONE] else (dtob (n/2))@[ZERO]
let bmul : bin -> bin -> bin 
= fun b1 b2 -> (dtob ((btod b1 (length b1))*(btod b2 (length b2))))
