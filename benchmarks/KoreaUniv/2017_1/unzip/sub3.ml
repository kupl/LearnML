  (*Problem 7*)
let fst p = match p with (x,_) -> x;;
let snd p = match p with (_,y) -> y;;

let rec concat f l =
match l with
|[] -> []
|hd::tl -> (f hd)::(concat f tl);;

let unzip l = ((concat fst l),(concat snd l));;