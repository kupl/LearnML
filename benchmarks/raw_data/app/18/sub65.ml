let rec app : 'a list -> 'a list -> 'a list
= fun a b ->
  let rec drop_sub : 'a ->'a list -> 'a list =
    fun target lst ->
      match lst with
        |hd::tl -> if(hd=target) then drop_sub target tl else hd::(drop_sub target tl)
        |_ -> [] in
    let rec drop : 'a list ->'a list -> 'a list =
      fun a b ->
        match b with
          |hd::tl -> drop (drop_sub hd a) tl
          |_ -> a in
  b@(drop a b);;