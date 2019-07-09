let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 ->
  match l1 with
  | [] -> l2
  | hd::tl -> let l1 = l1@l2 in
  match l1 with
  []->[]
  |hd::tl -> if hd l1 = false then hd::(tl) else (app tl);;



app [4;5;6;7] [1;2;3;4];;