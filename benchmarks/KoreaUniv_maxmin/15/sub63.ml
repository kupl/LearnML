let rec max : int list -> int =fun l -> 

match l with 
|[]->0
|hd::tl -> lim isbig 0 l;;



let rec min : int list -> int =fun l ->  

match l with 
|[]->0
|hd::tl -> lim issmall 10 l ;;

let rec lim f return l =
match l with 
|[]->return 
|hd::tl -> lim f (f return hd) tl;;

let isbig x y =
if x>y then x
else y;;

let issmall x y =
if x>y then y
else x;;
