(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec todec b1 =
  let v0 = (lstleng b1)-1 in
  match b1 with
  []->0
  |hd::tl->
  if hd=ONE then (fastexpt 2 v0)+(todec tl)
else 0 + todec tl

let rec tobin num =
  match num with
  0->[]
  |n->
  if n mod 2=0 then (tobin (n/2))@[ZERO]
  else (tobin (n/2))@[ONE]


let bmul : bin -> bin -> bin
= fun b1 b2 -> tobin ((todec b1)*(todec b2))