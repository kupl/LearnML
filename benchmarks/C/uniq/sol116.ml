let uniq : 'a list -> 'a list
= fun lst ->
  let rec iscontain l k =
    match l with
      | [] -> false
      | hd::tl -> if hd = k then true else iscontain tl k in
  let rec temp l res =
    match l with
      | [] -> res
      | hd::tl -> if (iscontain res hd) then temp tl res
      else temp tl (res@[hd]) in
  temp lst [];;

uniq [5;6;5;4];;