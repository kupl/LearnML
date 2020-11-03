let rec sort l =
   match l with
     [] -> []
   | h::t -> insert h (sort t)
 and insert elt l =
   match l with
     [] -> [elt]
   | h::t -> if elt <= h then elt::l else h::insert elt t;;
   
let rec find e = function
  | [] -> false
  | h::t -> h = e || find e t;;

let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> sort(match l1 with
    | [] -> l2
    | h::t -> if find h l2 then app t l2
              else app t (h::l2));;

app [4;5;6;7] [1;2;3;4];;