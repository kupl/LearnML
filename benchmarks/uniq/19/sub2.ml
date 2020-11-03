let rec reverse l = 
  match l with
    | [] -> []
    | h::t -> (reverse t) @ [h];;

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

let uniq : 'a list -> 'a list
= fun lst -> uniq_ lst;;

uniq [5;6;5;5;5;6;6;4;4];;
