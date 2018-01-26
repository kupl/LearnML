(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin
= fun b1 b2 -> let rec reverse l = match l with
    |[] -> []
    |hd::tl -> (reverse tl) @ hd ::[] in 
    let rec decimalize : bin -> int 
    = fun l -> match l with
     | [] -> 0
     | h::t -> let num = if h = ZERO then 0 else 1
      in num + 2 * (decimalize t) in
       let decimul : bin -> bin -> int 
       = fun b1 b2 -> let d1 = decimalize (reverse b1) in 
                      let d2 = decimalize (reverse b2) in
              d1 * d2
        in let rec binarize : int -> bin 
        = fun decimul -> match decimul with
        | 0 -> []
        | i -> let digi = (decimul mod 2) in
              if digi = 0 then binarize (decimul/2) @ [ZERO]
              else binarize (decimul/2) @ [ONE]
               in binarize (decimul b1 b2)