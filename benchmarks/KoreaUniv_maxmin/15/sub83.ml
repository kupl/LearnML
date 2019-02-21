let rec max l= match l with | []->0 | hd::tl -> if  hd < (max tl) then (max tl) else hd ;;

let rec min l= match l with | hd::tl -> if tl==[] then hd else if hd > (min tl) then (min tl) else hd | [hd] -> hd ;;
