let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> let l3= l2 @ l1 in match l3 with
  |[]->[]
  |hd::[]->hd::[]
  |hd1::hd2::r->if hd1=hd2 then app r [hd2]  else hd1::(app r [hd2]);;
  
  app [4;5;6;7][1;2;3;4];;