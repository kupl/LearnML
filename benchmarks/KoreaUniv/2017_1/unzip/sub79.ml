let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst ->
  match lst with 
  | [] -> ([],[])
  | h :: t -> 
  let rec  makeList : 'a * 'b -> ('a *'b) list -> 'a list * 'b list -> 'a list * 'b list
  = fun (a,b) lst (alst,blst) ->
    match lst with
    | [] -> (alst @ [a],blst @ [b])
    | h :: t -> makeList h t (alst @ [a], blst @ [b])
  in makeList h t ([],[]);;
