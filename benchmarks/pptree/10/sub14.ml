type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
            | Poland | Portugal | Italy | Germany | Sweden | England
            | Croatia | Argentina
type tourna = LEAF of team
              | NODE of tourna * tourna
exception Error of string

let rec print s_l_l =
  match s_l_l with
    h::t::[] -> 
    (
      let _ = (List.iter (print_string) h) in
      print_string "\n"
    )
    |h::t1::t2 ->
    (
      let _ = (List.iter (print_string) h) in
      let _ = print_string "\n" in
      print (t1::t2)
    )
    |_ -> raise (Error "Doesn't exist case")

let rec list_gen i ele= 
  if (i <= 0) then []
  else ele::(list_gen (i-1) ele)

let rec attach a b space = 
  match (a,b) with
    (h1::[],h2::[]) -> (h1@(list_gen space (" "))@h2)::[]
    |(h1::t1,h2::t2) -> (h1@(list_gen space (" "))@h2)::(attach t1 t2 space)
    |(_,_) -> raise (Error "Doesn't exist case")

let calc_center l = 
  let rec numberofspace l =
    match l with
      [] -> 0
      |h::t -> if (h = " ") then 1 + (numberofspace t) else 0 
  in
  ((List.length (List.filter (fun x -> (x = "-")) l))+1) / 2 + (numberofspace l) 

let merge_2 l r =
  let l0 = List.nth l 0 in
  let r0 = List.nth r 0 in
  let leftspace = (calc_center l0) in
  let rightspace = (List.length r0) - (calc_center r0) - 1 in
  let underbar = 
    if ((List.length l0) - (calc_center l0) + (calc_center r0)) mod 2 = 0 then
      ((List.length l0) - (calc_center l0) + (calc_center r0)) + 1
    else
      ((List.length l0) - (calc_center l0) + (calc_center r0)) 
  in
  let newline = (list_gen leftspace (" "))@["|"]@(list_gen underbar ("-"))@["|"]@(list_gen rightspace (" ")) in
  let space = (List.length newline) - (List.length l0) - (List.length r0) in
  newline::(attach l r space)

let merge_1 l r =
  let height_l = List.length l in
  let height_r = List.length r in
  if (height_l > height_r) then
    let r = 
      (list_gen (height_l - height_r) ((list_gen (calc_center (List.nth r 0)) (" "))@["|"]))@r
    in
    (merge_2 l r)
  else if (height_l < height_r) then
    let l = 
      (list_gen (height_r - height_l) ((list_gen (calc_center (List.nth l 0)) (" "))@["|"]))@l
    in
    (merge_2 l r)
  else
    (merge_2 l r)

let finalize sll = 
  ((list_gen (calc_center (List.nth sll 0)) (" "))@["|"])::sll

  let rec make t =
  match t with
    LEAF _ -> [[]]
    |NODE (a,b) -> (merge_1 (make a) (make b))

let pptree t =
  print (finalize (make t))
