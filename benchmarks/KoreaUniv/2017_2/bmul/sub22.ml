  (*problem 7*)
  type digit = ZERO | ONE
  type bin = digit list

  let bmul
  = fun b1 b2 ->
  let rec power = fun b -> match b with
  | 0 -> 1
  | _ -> 2*(power (b-1)) in
  let rec getlen = fun b -> match b with
  | [] -> 0
  | hd::tl -> 1+ (getlen tl) in
  let rec btod = fun b -> match b with
  | [] -> 0
  | hd::tl -> (match hd with
      | ZERO -> 0+(btod tl)
      | ONE -> (power((getlen (hd::tl))-1)) + (btod tl) ) in 
  let rec dtob = fun d -> match (d mod 2) with
  | 0 -> if (d/2)=0 then [ONE] else (dtob(d/2))@[ZERO]
  | 1 -> if (d/2)=0 then [ONE] else (dtob(d/2))@[ONE] in
  (dtob((btod(b1))*(btod(b2))));;

