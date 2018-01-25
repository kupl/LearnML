let rec unzip lst =
  let fst a = match a with (x,_) -> x in
  let snd a = match a with (_,x) -> x in
  match lst with
  |hd::tl -> ((fst hd)::(fst (unzip tl)), ((snd hd)::(snd (unzip tl))))
  |_ -> ([],[])