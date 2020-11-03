let rec search : 'a -> 'a list -> 'a list -> 'a list
= fun c ls temp ->
  match ls with
    | [] -> temp
    | hd::tl -> if hd = c then search c tl temp else search c tl temp@[hd]

let app : 'a list -> 'a list -> 'a list
= fun l1 l2 ->
  
  
  let rec trim : 'a list -> 'a list -> 'a list
  =fun ls1 ls2 ->
    match ls2 with 
      | [] -> ls1
      | hd::tl -> trim (search hd ls1 []) tl 
  
  
  in l2@(trim l1 l2)

;;

app [4;5;6;7] [1;2;3;4];;
app [1;2;3] [2;1]
;;
app [1;2;3;4;5;6;7;4;3;2] [3;4;2;2;5;2;3;3;5;7;7;2;9;12;3;12;31;32;1];;