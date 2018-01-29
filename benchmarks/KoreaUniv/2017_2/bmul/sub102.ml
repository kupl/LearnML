
(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec getpowertwo : int -> int
= fun i ->
  match i with
  |1 -> 1
  |_ -> 2*getpowertwo (i-1)

let rec getinteger : bin -> int ->int
= fun bl two->
  match bl with
  | [] -> 0
  | hd::tl -> 
    (match hd with
     |ZERO -> getinteger tl (two/2)
     |ONE -> two + getinteger tl (two/2)
    )

let rec getbin : int -> bin
=fun i ->
  match i with
  |0 -> [ZERO]
  |1 -> [ONE]
  |_ ->
    (match (i mod 2) with
      |0 -> [ZERO]@(getbin (i/2))
      |1 -> [ONE]@(getbin ((i-1)/2))
      |_ -> raise Not_implemented
    )

let bmul : bin -> bin -> bin
= fun b1 b2 -> (* TODO *)
  let len1 = List.length b1 in
  let len2 = List.length b2 in
  let two1 = getpowertwo len1 in
  let two2 = getpowertwo len2 in
  let realnum1 = getinteger b1 two1 in
  let realnum2 = getinteger b2 two2 in
  let beforeanswer= realnum1*realnum2 in
  let beforebin = getbin beforeanswer in
  List.rev beforebin

