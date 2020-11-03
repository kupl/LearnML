let rec reverse l = 
  match l with
    | [] -> []
    | h::t -> (reverse t) @ [h];;

let rec insert a l =
  match l with
    | []->[a]
    | h::t -> if a<h then a::h::t
              else h:: (insert a t);;

let rec sort l =
  match l with
    |[]->[]
    |h::t->insert h (sort t);;

let remove x l =
  let rec f l emp = match l with
    | [] -> reverse emp
    | h::t when x = h -> f t emp
    | h::t -> f t (h::emp)
  in f l []

let uniq_ l =
  let rec f l emp = match l with
    | [] -> reverse emp
    | h::t -> f (remove h t) (h::emp)
  in f l [];;

let app_ l1 l2 =
  let l3 = l1@l2 
  in let l4 = sort l3
  in uniq_ l4;;
  
  
let app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> app_ l1 l2;;

app [3;2;4;6;1;2] [1;3;5;7;2;5];;
