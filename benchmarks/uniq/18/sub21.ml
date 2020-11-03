let rec remove hd tl = 
  match hd, tl with
    | hd, hd1 :: tl1 -> if hd = hd1 then remove hd tl1 else [hd1] @ (remove hd tl1)
    | _, [] -> [] ;;
    
let rec uniq : 'a list -> 'a list
= fun lst -> 
  match lst with 
    | hd :: tl -> [hd] @ uniq (remove hd tl)
    | [] -> [] ;;