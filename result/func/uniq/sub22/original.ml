let rec uniq (lst : 'a list) : 'b list =
  let rec member x (lst : 'a list) : bool =
    match lst with
    | [] -> false
    | hd :: tl -> if hd = x then true else member x tl
  in

  match lst with
  | [] -> []
  | hd :: tl -> if member hd tl = true then uniq tl else hd :: uniq tl
