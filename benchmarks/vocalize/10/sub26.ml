exception Error of string

let table1 = ["¿µ" ; "ÀÏ" ; "ÀÌ" ; "»ï"; "»ç" ; "¿À" ; "À°" ; "Ä¥" ; "ÆÈ" ; "±¸"]
let table2 = ["ÀÏ" ; "½Ê" ; "¹é" ; "Ãµ"]

let debug lst = List.map (function lst -> (List.map print_string lst) ; (print_string " ")) lst

let rec remove_tail lst = match lst with 
[] -> raise (Error "remove_tail error")
| [a] -> []
| a::h -> [a]@ (remove_tail h)

let trans str =
let rec trans_in str = 
let len = String.length str in
if (len > 4 || len < 0) then (raise (Error "String's length over"))
else if (len=0) then [] 
else if (len=1) then 
(let num = (int_of_string (String.sub str 0 1)) in
if (num=0) then []
else if (num=1) then [(List.nth table1 num)]
else [(List.nth table1 num)])
else
(let num = (int_of_string (String.sub str 0 1)) in
if (num=0) then (trans_in (String.sub str 1 (len-1)))
else if (num=1) then [(List.nth table2 (len-1))]@(trans_in (String.sub str 1 (len-1)))
else [(List.nth table1 num)]@[(List.nth table2 (len-1))]@(trans_in (String.sub str 1 (len-1)))) in
let lst = trans_in str in
if (lst=[]) then ["¿µ"] else lst

let vocalize str =
let error_check = try (int_of_string) str with (Failure "int_of_string") -> raise (Error "input Type Error") in
let len = String.length str in
if (len > 8 || len < 7) then (raise (Error "Input Size Error"))
else [trans (String.sub str 0 (len-4))]@[trans (String.sub str (len-4) 4)];;
