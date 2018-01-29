(* problem 7*)
type digit = ZERO | ONE
type bin = digit list
;;

let rec getsize b = 
match b with
| []->0
| hd::tl -> 1+(getsize tl);;

let rec pow n =
match n with
| 0 -> 1
| _ -> 2*(pow (n-1));;

let rec btod b =
match b with
| []->0
| hd::tl -> (match hd with 
            | ZERO -> (btod tl)
            | ONE -> (pow ((getsize b)-1)) + (btod tl));;

let rec dtob n b =
if n=0 then b
else
  if (n mod 2)=0 then (dtob (n/2) (ZERO::b))
  else (dtob (n/2) (ONE::b));;

let bmul : bin -> bin -> bin
= fun b1 b2 ->
if (btod b1)=0 || (btod b2)=0 then [ZERO]
else dtob ((btod b1)*(btod b2)) [] ;;

