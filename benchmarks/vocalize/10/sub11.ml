exception Error of string

let slist = ["ÀÏ"; "ÀÌ"; "»ï"; "»ç"; "¿À"; "À°"; "Ä¥"; "ÆÈ"; "±¸"]
let olist = ["Ãµ"; "¹é"; "½Ê"]

(* extends 7-length string to 8-length string *)
let attach str = 
  if String.length str = 7 then
  let newstr = (String.create 8) in
  String.blit str 0 newstr 1 7;
  String.set newstr 0 '0';
  newstr
  else str

(* change char to int *)
let change char =
  let value = int_of_char char - int_of_char '0' in
  if value = 0 then ""
  else List.nth slist (value - 1)

(* make substring *)
let substr1 str = String.sub str 0 4
let substr2 str = String.sub str 4 4

(* make list of substring *)
let rec make_list sstr index =
  let tmp = change (String.get sstr index) in
  if index < 3 then
  if tmp = "" then make_list sstr (index+1)
  else if tmp = "ÀÏ" then (List.nth olist index)::(make_list sstr (index+1))
  else tmp::(List.nth olist index)::(make_list sstr (index+1))
  else
  if tmp = "" then []
  else tmp::[];;

let vocalize str =
  if String.length str = 7 || String.length str = 8 then
  let newstr = attach str in
  let list1 = make_list (substr1 newstr) 0 in
  let list2 = make_list (substr2 newstr) 0 in
  if list1 = [] then
    if list2 = [] then [["¿µ"]; ["¿µ"]]
    else [["¿µ"]; list2]
  else
    if list2 = [] then [list1; ["¿µ"]]
    else [list1; list2]
  else raise (Error "invalid length")

(*
let rec print_list list =
  match list with s::t -> print_string s; print_list t
  | [] -> print_string ""

let rec print_list_list list =
  match list with s::t -> print_list s; print_string " "; print_list_list t
  | [] -> print_list []  

let _ = print_list_list (vocalize "80010000")
*)