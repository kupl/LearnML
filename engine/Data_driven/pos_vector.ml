open Lang
open Node
open Mapper

exception Table_Update_Failure

module Fv = Freq_vector
(*node with height*)
type t = (int BatVect.t) list

let init_vector = List.map (fun (s,num) -> 
    (s, BatVect.empty)) Fv.init_vector 

let init_tbl = BatHashtbl.of_list init_vector
let gen_tbl () = BatHashtbl.copy init_tbl

module Pos_map = 
  Make_genmap(struct 
    type t = int BatVect.t
    let init_vector = init_vector
  end)

let update : (string, int BatVect.t) BatHashtbl.t -> string -> int -> unit
= fun tbl s h -> 
  let cur_v = 
    try BatHashtbl.find tbl s
    with Not_found -> raise (Failure "pos vec update: Not found key") in
  BatHashtbl.replace tbl s (BatVect.append h cur_v)

let rec traverse : (string, int BatVect.t) BatHashtbl.t -> node -> unit
= fun tbl node ->
  match node with
  | LNode (l,n) -> traverse tbl n
  | Node (h,s,lst) -> update tbl s h;
    List.iter (traverse tbl) lst
  | Leaf -> ()
  | Empty -> raise (Failure "pos_vec traverse: Empty node")

let rec node_height_cal : node -> node
= fun node ->
  let rec height : node -> int 
  = fun n -> 
    begin match n with 
      | Leaf -> 0
      | Node(h,s,lst) as l -> begin
        try (1 + BatList.max (List.map height lst))
        with _ -> raise (Failure (node_to_string l))
        end
      | LNode(l,n) -> height n 
      | Empty -> raise (Failure "posvec node_height_cal: node Empty") end
    in
  match node with
  | Node(h,s,lst) as n-> Node (height n,s,lst)
  | LNode(l,n) -> node_height_cal n
  | _ -> node 
  
let node_vectorize : node -> t 
= fun node -> 
  let table = gen_tbl () in
  let hnode = node_height_cal node in traverse table hnode;
  BatHashtbl.to_list table |> List.sort compare |> List.map (fun (k,v) -> v)

let rec funcs_vectorize : (string * lexp) list -> (string * t) list
= fun lst ->
  match lst with
  | [] -> []
  | (f,lexp)::tl ->
    let vec = lexp |> exp_to_node |> node_vectorize in
    (f, vec) :: (funcs_vectorize tl)

let calculate_distance : t -> t -> float
= fun t1 t2 ->
  let aux_calc : float -> int BatVect.t -> int BatVect.t -> float
  = fun acc v1 v2 ->
    let l1 = BatVect.to_list v1 |> List.sort compare in
    let l2 = BatVect.to_list v2 |> List.sort compare in 
    acc +. Freq_vector.calculate_distance l1 l2 in 
  
  List.fold_left2 (fun acc v1 v2 -> 
    let vlen1 = BatVect.length v1 in
    let vlen2 = BatVect.length v2 in

    if vlen1 = vlen2 then aux_calc acc v1 v2 
    else if vlen1 > vlen2 then 
      let padding = BatVect.make (vlen1-vlen2) 0 in
      let v2' = BatVect.concat v2 padding in
      aux_calc acc v1 v2'
    else 
      let padding = BatVect.make (vlen2-vlen1) 0 in
      let v1' = BatVect.concat v1 padding in
      aux_calc acc v1' v2
  ) 0.0 t1 t2 

let rec gen_score_map : (string * t) list -> (string * t) list -> ((string * string) * float) list 
= fun ts1 ts2 ->
  match ts1 with
  | [] -> []
  | (s,v)::t ->
    let with_s = List.map (fun (s',v') -> let dist = calculate_distance v v' in 
    (s,s'), dist) ts2 in with_s @ gen_score_map t ts2

let calculate_mapping_distance : (string*t) list -> (string*t) list -> (string * string) list * float 
= fun ts1 ts2 ->
  let ts1,ts2 = Pos_map.padding ts1 ts2 in
  let all_func_mapping = Pos_map.gen_mapping ts1 ts2 in
  let score_map = gen_score_map ts1 ts2 in
  let calculate_func_score = List.fold_left (fun acc (s,t,s',t') -> 
                               let score = List.assoc (s,s') score_map in 
                               acc +. score) 0.0 in
  let key_filter = (fun (s,t,s',t') -> s,s') in 
  let min_mapping = List.fold_left (fun (min_map,min) cur_map -> 
                      let cur_score = calculate_func_score cur_map in
                      if min > cur_score then ((List.map key_filter cur_map), cur_score)
                      else (min_map, min)) 
                    ([],max_float) all_func_mapping 
  in min_mapping
    
let search_solutions_by_function_match : int -> prog -> (string * prog) list -> (string * prog * ((string * string) list * float)) list
= fun topk sub solutions ->
  let vectorize = funcs_vectorize in
  let calculate = calculate_mapping_distance in
  let preproc = (fun x -> x |>  Extractor.extract_func_all |> vectorize) in
  let v_sub = preproc sub in
  let topk_lst = List.map (fun (f, sol) -> (f, sol, (preproc sol))) solutions |>
                 List.map (fun (f ,sol, v_sol) -> (f, sol, (calculate v_sub v_sol))) |>
                 List.sort (fun (_,_,(_,dist)) (_,_,(_,dist')) -> compare dist dist') |>
                 BatList.take topk in
  topk_lst
    
let search = search_solutions_by_function_match

