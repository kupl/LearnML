let  lst2int : int list -> int
= fun lst -> (*TODO*)
  let rec listoint lst num=
    match lst with
     [] -> num
    |hd::tl -> listoint tl (num*10+hd)
  in listoint lst 0;; 

