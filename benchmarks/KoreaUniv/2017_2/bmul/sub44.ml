(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin
= fun b1 b2 ->
  let rec createdec b pot =
    match b with
    |[] -> 0
    |hd::tl -> (match hd with
                |ZERO -> createdec tl (pot*2)
                |ONE -> (1*pot)+(createdec tl (pot*2)))
  in let res = (createdec (List.rev b1) 1) * (createdec (List.rev b2) 1)
  in if res = 0 then [ZERO] else 
    let rec createbin b =
        match b with
        |0 -> []
        |_ -> (createbin (b/2))@(if b mod 2 = 0 then [ZERO] else [ONE])
  in createbin res;;


