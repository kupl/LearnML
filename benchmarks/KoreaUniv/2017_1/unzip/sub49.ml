(* problem 7*)

let rec unzip : ('a * 'b) list -> 'a list * 'b list = fun lst ->
  let fst (x , _) = x in let snd (_ , x) =x in
  match lst with
  | [] -> ([], [])
  | hd::tl -> 
    let l = unzip tl in
    ((fst hd)::(fst l), (snd hd)::(snd l));;