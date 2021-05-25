let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> (* TODO *)
  let rec loop l n = 
    match l with
    | [] -> l
    | hd::tl -> 
      if (n = hd) then (loop tl n)
      else hd::(loop tl n) in
  match l2 with
  | [] -> l1
  | hd::tl ->
    let rec outloop lst1 lst2 =
      match lst2 with
      | [] -> lst1
      | hd2::tl2 -> outloop (loop lst1 hd2) tl2
    in l2@(outloop (loop l1 hd) tl) 
;;
    

app [5;6;7;8;9;10;11] [5;6;7;9];;
app [4;5;6;7] [1;2;3;4];;

(*let app : 'a list -> 'a list -> 'a list*)
(*= fun l1 l2 -> (* TODO *)*)