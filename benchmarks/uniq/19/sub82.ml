let rec comp h lst =
  match lst with
    [] -> true
    |hd::tl -> if (h == hd) then false
              else comp h tl

let rec CreateNewList (newl : 'a list) (orgl : 'a list) =
  match orgl with
    [] -> newl
    |hd::tl -> if (comp hd newl) then CreateNewList (newl @ [hd]) tl
              else CreateNewList newl tl

let uniq : 'a list -> 'a list
= fun lst -> 
CreateNewList [] lst;;