let zipperN l = 
  let rec zipperIn e = 
   match e with
    | [], [] -> []
    | []::t, l1 -> zipperIn(t, l1)
    | [],  l1 -> zipperIn(l1, [])
    | (h::t1)::t2, l1 -> h::zipperIn(t2, (l1@[t1]))
  in
    zipperIn (l, [])


