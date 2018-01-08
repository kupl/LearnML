(* import code from hw1_4 written by Junha Roh *)
exception Error of string

type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
  | Poland | Portugal | Italy | Germany | Sweden | England
  | Croatia | Argentina

type tourna = LEAF of team
  | NODE of tourna * tourna

(* return the total level of the tree *)
let rec level t =
  match t with LEAF l -> 1
  | NODE(a, b) ->
  let rec max a b = if a > b then a else b in
  max ((level a)+1) ((level b)+1)

(* make empty string list with n elements *)
let rec str_array n = if n > 0 then [""]@(str_array (n-1)) else []

(* copy the given string n times *)
let rec copy_str str n = if n > 0 then str ^ (copy_str str (n-1)) else ""

(* apply function f to the list[index] and return a new list *)
let rec at list index f = if index > (List.length list) then raise (Error "out of bound")
  else if index = 0 then ([f (List.hd list)])@(List.tl list)
  else [(List.hd list)]@(at (List.tl list) (index-1) f)

(* return the distance between tree *)
let rec dist n =
  if n = 1 then 1
  else if n > 1 then 2 * (dist (n-1)) + 1
  else 0

(* return the amount of shift *)
let rec shift n =
  if n = 1 then 1
  else if n > 1 then 2 * (shift (n-1))
  else 0

(* return the amount of shift *)
let pos_shift t = shift ((level t)-1)

(* return the position of the center(VBAR) of the tree *)
let rec center t =
  match t with LEAF l -> 1
  | NODE(a, b) ->
  let n = if (level a) > (level b) then (level a) else (level b) in
  let d = (dist n) in
  (center a) + (d+1)/2

(* make tuple list *)
let rec mtree t pos =
  match t with LEAF l -> [(1, pos, 1, '|')]
  | NODE(a, b) -> [((level t), pos, 1, '|')] @ (mtree a (pos - (pos_shift t))) @ [((level t)-1, (pos - (pos_shift t) + 1), (dist ((level t)-1)), '-')] @ (mtree b (pos + (pos_shift t)))

let rec mtree t pos =
  match t with LEAF l -> [(1, pos, 1, '|')]
  | NODE(a, b) ->
    let vbar = [((level t), pos, 1, '|')] in
    let bar = [((level t)-1, (pos - (pos_shift t) + 1), (dist ((level t)-1)), '-')] in
    let posLeft = (pos - (pos_shift t)) in
    let left = [((level t)-1, posLeft, 1, '|')] in
    let right = [((level t)-1, (pos - (pos_shift t) + (dist ((level t)-1)))+1, 1, '|')] in
    let rightTree = (mtree b (pos + (pos_shift t))) in
    let leftTree = (mtree a (pos - (pos_shift t))) in
    if posLeft < 0 then vbar @ leftTree @ bar @ right @ rightTree
    else vbar @ leftTree @ left @ bar @ right @ rightTree

(* compare tuples *)
let comp (lev1, pos1, rep1, char1) (lev2, pos2, rep2, char2) =
  if lev1 = lev2 then
  if pos1 = pos2 then 0
  else if pos1 < pos2 then 1
  else -1
  else if lev1 > lev2 then 1
  else -1

(* make tuple list with tree and sort *)
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

let rec gentree tlist strlist =
  match tlist with [] -> strlist
  | (lev, pos, rep, chr)::t -> gentree t ( at strlist (lev-1) (change (lev, pos, rep, chr)) )

let isBar str index = if index >=0 && index < (String.length str) then if str.[index] = '|' then true else false else false
let isLeftGood str index = if (index > 0 && str.[index-1] = '-') then true else false
let isRightGood str index = if (index < (String.length str - 1) && str.[index+1] = '-') then true else false
let rec makeBadListRec str index =
  if index >=0 && index < (String.length str) then
    if (isBar str index) && (not (isLeftGood str index) && not (isRightGood str index)) then index::(makeBadListRec str (index+1))
    else makeBadListRec str (index+1)
  else []
let makeBadList str = makeBadListRec str 0
let rec makeGoodBarRec strlist h index =
  if h >= 0 && h < List.length strlist then
  match (List.nth strlist h).[index] with '|' -> ()
  | ' ' -> (List.nth strlist h).[index] <- '|'; makeGoodBarRec strlist (h-1) index
  | _ -> ()
  else ()
let makeGoodBar strlist index = makeGoodBarRec strlist ((List.length strlist)-2) index
let rec goodBar badlist strlist =
  match badlist with [] -> ()
  | h::t -> makeGoodBar strlist h; goodBar t strlist
let complete strlist =
  let badlist = makeBadList (List.nth strlist ((List.length strlist)-1)) in
  goodBar badlist strlist

let pptree t =
  let tlist = ptree t in
  let h = level t in
  let result = gentree tlist (str_array h) in
  let rec print_list strlist =
    match strlist with [] -> ()
    | h::t -> print_endline h; print_list t in
  complete result;
  print_list (List.rev result)

let a = LEAF Korea
let b = LEAF Japan
let c = NODE (a, b)
let d = NODE (c, a)
let e = NODE (b, c)
let f = NODE (c, c)
let g = NODE (d, a)
let h = NODE (a, d)
let i = NODE (a, g)
let j = NODE (h, d)
let k = NODE (i, j)
let l = NODE (c, c)
let m = NODE (l, l)
let n = NODE (m, m)
let o = NODE (n, n)
let p = NODE (o, o)

let _ =
  print_endline "a";
  pptree a;
  print_endline "b";
  pptree b;
  print_endline "c";
  pptree c;
  print_endline "d";
  pptree d;
  print_endline "e";
  pptree e;
  print_endline "f";
  pptree f;
  print_endline "g";
  pptree g;
  print_endline "h";
  pptree h;
  print_endline "i";
  pptree i;
  print_endline "j";
  pptree j;
  print_endline "k";
  pptree k;
  print_endline "l";
  pptree l;
  print_endline "m";
  pptree m;
  print_endline "n";
  pptree n;
  print_endline "o";
  pptree o;
  print_endline "p";
  pptree p;;