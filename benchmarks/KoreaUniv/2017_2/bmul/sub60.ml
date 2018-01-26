(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec length l =
  match l with
    | [] -> 0
    | h::t -> length t + 1

let rec btod : bin * int -> int
= fun (b, len)->
  match b with
    | [] -> 0
    | h::t -> 
      if (h = ZERO) then btod(t, length t)
      else (expt 2 (len-1) + btod(t, length t))
  
let rec dtob : int -> bin
= fun integer -> 
  if (integer = 0) then [ZERO]
  else if (integer = 1) then [ONE]
  else if (integer mod 2 = 0) then dtob(integer/2)@[ZERO]
  else dtob(integer/2)@[ONE]

let bmul : bin -> bin -> bin
= fun b1 b2 -> (* TODO *)
  dtob (btod (b1, length b1) * btod (b2, length b2))