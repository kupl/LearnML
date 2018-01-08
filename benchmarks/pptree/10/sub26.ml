type team = Korea | France | Usa | Brazil
	  | Japan | Nigeria | Cameroon | Poland
	  | Portugal | Italy | Germany | Sweden
	  | England | Croatia | Argentina
type tourna = LEAF of team
            | NODE of tourna * tourna

let rec mk_pp_list (cur,total) args =
 let rec getN level =
  if level = 0 then 0
  else (int_of_float (2. ** (float_of_int (level-1)))) + (getN (level-1))
 in
 let rec iter_concat (elem, cnt) =
   if cnt = 0 then [elem]
   else elem::(iter_concat (elem, cnt-1))
 in
 let rec makespace n =
  if n = 0 then []
  else " "::(makespace (n-1))
 in
 let rec makebar n =
  if n = 0 then []
  else "-"::(makebar (n-1))
 in
 let mk_leaf_graph level =
  let blank = (makespace (getN level)) in
  let elem = blank@["|"]@blank in
  (iter_concat (elem, level))
 in
 let concat_b alist blist = alist @ [" "] @ blist
 in
 match args with
 (LEAF _) -> (mk_leaf_graph (total - cur))
 |(NODE (t1, t2)) ->(let t1graph = (mk_pp_list (cur+1, total) t1) in
 		     let t2graph = (mk_pp_list (cur+1, total) t2) in
		     let num = (getN (total - cur)) in
		     let nodeblank = (makespace (num/2)) in 
		     let resulthd = nodeblank@["|"]@(makebar num)@["|"]@nodeblank in
		     let resulttl = (List.map2 concat_b (List.tl t1graph) (List.tl t2graph)) in
		     let rootspace = (makespace num) in
		     let rootgraph = rootspace@["|"]@rootspace in
		     (rootgraph::[resulthd]@resulttl))


let pptree tourna_arg =
 let rec getheight args =
  match args with
  (LEAF _) -> 0
  |(NODE (t1, t2)) -> (let t1height = (getheight t1) in
 	               let t2height = (getheight t2) in
		       if (t1height > t2height) then (t1height+1) else (t2height+1))
 in
 let rec print_pp_list pplist = 
  match pplist with
  [] -> (print_string "")
  |h::t -> (List.iter print_string h);(print_string "\n");(print_pp_list t)
 in
 let pplist = (mk_pp_list (0, (getheight tourna_arg)) tourna_arg) in
 (print_pp_list pplist)

