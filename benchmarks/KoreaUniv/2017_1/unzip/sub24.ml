(* HW7 *)
  let fst p = match p with (x,_) -> x;;
  let snd p = match p with (_,x) -> x;;
  
  let rec hdapp l =
  match l with 
  | [] -> []
  | hd::tl -> (fst hd)::(hdapp tl)

  let rec tlapp l =
  match l with
  | [] -> []
  | hd::tl -> (snd hd)::(tlapp tl)

  let rec unzip l =
  match l with
  | [] -> []
  | hd::tl ->[hdapp l,tlapp l]