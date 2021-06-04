let rec uniq : 'a list -> 'a list =
 fun lst ->
  let rec member x lst =
    match lst with
    | [] -> false
    | hd :: tl -> if hd = x then true else member x tl
  in

  match lst with
  | [] -> []
  | hd :: tl -> if member hd tl = true then uniq tl else hd :: uniq tl
