
(* 7 *)
let rec unzip l=
match l with
   [] -> ([], [])
  |(x,y)::tl ->
    let tuple = unzip tl in
    ((x::(fst tuple)), (y::(snd tuple)));;
