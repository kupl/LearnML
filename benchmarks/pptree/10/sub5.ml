(* import code from hw1_4 written by Junha Roh *)
exception Error of string

type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
  | Poland | Portugal | Italy | Germany | Sweden | England
  | Croatia | Argentina

type tourna = LEAF of team
  | NODE of tourna * tourna

type graph = BLANK of int
  | BAR of int
  | VBAR
  | ENDLINE

let rec level t =
  match t with LEAF l -> 1
  | NODE(a, b) ->
  let rec max a b = if a > b then a else b in
  max ((level a)+1) ((level b)+1)

let rec range n = if n > 0 then []::(range (n-1)) else []

let rec str_array n = if n > 0 then [""]@(str_array (n-1)) else []

let rec copy_str str n = if n > 0 then str ^ (copy_str str (n-1)) else ""

let rec at list index f = if index > (List.length list) then raise (Error "out of bound")
  else if index = 0 then ([f (List.hd list)])@(List.tl list)
  else [(List.hd list)]@(at (List.tl list) (index-1) f)

let rec dist n =
  if n = 1 then 1
  else if n > 1 then 2 * (dist (n-1)) + 1
  else 0

let rec shift n =
  if n = 1 then 1
  else if n > 1 then 2 * (shift (n-1))
  else 0

let pos_shift t = shift ((level t)-1)

let rec center t =
  match t with LEAF l -> 1
  | NODE(a, b) ->
  let n = if (level a) > (level b) then (level a) else (level b) in
  let d = (dist n) in
  (center a) + (d+1)/2

let rec mtree t pos =
  match t with LEAF l -> [(1, pos, 1, '|')]
  | NODE(a, b) -> [((level t), pos, 1, '|')] @ (mtree a (pos - (pos_shift t))) @ [((level t)-1, (pos - (pos_shift t) + 1), (dist ((level t)-1)), '-')] @ (mtree b (pos + (pos_shift t)))

let comp (lev1, pos1, rep1, char1) (lev2, pos2, rep2, char2) =
  if lev1 = lev2 then
  if pos1 = pos2 then 0
  else if pos1 < pos2 then 1
  else -1
  else if lev1 < lev2 then 1
  else -1

let ptree t = List.sort comp (mtree t (center t))

(* string modifier *)
let modify t str = match t with (lev, pos, rep, chr) -> (* print_endline ("modify: " ^ str); *)
  String.fill str (pos - 1) rep chr; str

(* extend the string and return the result to the given function *)
let extender t str f =
  match t with (lev, pos, rep, chr) ->
  let newstr = String.make (pos + rep - 1) ' ' in
  String.blit str 0 newstr 0 (String.length str);
  (* print_endline ("extender: " ^ newstr); *)
  f t newstr

(* change string with given tuple *)
let change t str =
  match t with (_, pos, rep, chr) ->
  if (String.length str) > (pos + rep - 1) then modify t str
  else extender t str modify

let makelist list =
  match list with [] -> []
  | (lev, _, _, _)::t -> str_array lev  

let rec gentree tlist strlist =
  match tlist with [] -> strlist
  | (lev, pos, rep, chr)::t -> gentree t ( at strlist (lev-1) (change (lev, pos, rep, chr)) )

let pptree t =
  let tlist = ptree t in
  let h = level t in
  let result = List.rev (gentree tlist (str_array h)) in
  let rec print_list strlist =
    match strlist with [] -> ()
    | h::t -> print_endline h; print_list t in
  print_list result

let tmptree t =
  let tlist = ptree t in
  let h = level t in
  List.rev (gentree tlist (str_array h))