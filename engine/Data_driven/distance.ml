module FV = Freq_vector
module PV = Pos_vector

let ins_all_positions x l = 
  let rec aux prev acc = function
    | [] -> (prev @ [x]) :: acc |> List.rev
    | hd::tl as l -> aux (prev @ [hd])((prev @ [x] @ l) :: acc) tl
  in aux [] [] l

let rec permutations = function
  | [] -> []
  | x::[] -> [[x]]
  | x::xs -> List.fold_left (fun acc p -> acc @ ins_all_positions x p) [] (permutations xs)

let gen_mapping : (string * 'a) list -> (string * 'a) list -> (string * 'a * string * 'a) list list
= fun ts1 ts2 ->
  let len1 = List.length ts1 in
  let len2 = List.length ts2 in
  if len1 = len2 then 
    let perms = permutations ts2 in 
    List.fold_left (fun acc y -> 
      (List.map2 (fun (s,t) (s',t') -> (s,t,s',t')) ts1 y)::acc) [] perms
  else 
    raise (Failure "func map length must be same")

let rec gen_score_map : 'a -> (string * 'a) list -> (string * 'a) list -> ((string * string) * float) list 
= fun ts1 ts2 ->
  match ts1 with
  | [] -> []
  | (s,v)::t ->
    let with_s = List.map (fun (s',v') -> let dist = dist_func v v' in 
    (s,s'), dist) ts2 in with_s @ gen_score_map t ts2
    
let padding : 'a -> (string * 'a) list -> (string * 'a) list -> (string * 'a) list * (string * 'a) list
= fun ts1 ts2 ->
  let len1 = List.length ts1 in
  let len2 = List.length ts2 in
  (*how do handle polymorphic init_vector?
   * -> parameterize
   * *)
  let empty_padding = List.map (fun (k,v) -> v) init_vector in

  if len1 = len2 then ts1,ts2 
  else if len1 < len2 then 
    let rec iter = fun acc count -> 
    if count <> (len2-len1) then 
      iter (("___padding" ^ string_of_int(count+1), empty_padding)::acc) 
      (count+1) else acc in let ts1 = iter ts1 0 in ts1,ts2
  else 
    let rec iter = fun acc count ->
    if count <> (len1-len2) then 
      iter (("___padding"^string_of_int(count+1), empty_padding)::acc) 
      (count+1) else acc in let ts2 = iter ts2 0 in ts1,ts2

let calculate_mapping_distance : 'a ->  (string * 'a) list -> (string * 'a) list -> (string * string) list * float 
= fun dist_func ts1 ts2 ->
  let ts1,ts2 = padding ts1 ts2 in
  let all_func_mapping = gen_mapping ts1 ts2 in
  let score_map = gen_score_map dist_func ts1 ts2 in
  let calculate_func_score = List.fold_left (fun acc (s,t,s',t') -> 
                               let score = List.assoc (s,s') score_map in 
                               acc +. score) 0.0 in
  let key_filter = (fun (s,t,s',t') -> s,s') in 
  let min_mapping = List.fold_left (fun (min_map,min) cur_map -> 
                      let cur_score = calculate_func_score cur_map in
                      if min > cur_score then ((List.map key_filter cur_map), cur_score)
                                         else (min_map, min)) ([],max_float) all_func_mapping 
  in min_mapping
    
let search_solutions_by_program_match : int -> prog -> (string * prog) list -> (string * prog * float) list 
= fun topk sub solutions ->
  let vectorize = FV.prog_vectorize in 
  let calculate = calculate_distance in
  let v_sub = vectorize sub in
  let topk_lst = List.map (fun (f, sol) -> (f, sol, (vectorize sol))) solutions |> 
                 List.map (fun (f, sol, v_sol) -> (f, sol, (calculate v_sub v_sol))) |>
                 List.sort (fun (_,_,dist) (_,_,dist') -> compare dist dist') |>
                 BatList.take topk in
  topk_lst
    
let search_solutions_by_func_freqvec_match : int -> prog -> (string * prog) list -> (string * prog * ((string * string) list * float)) list
= fun topk sub solutions ->
  let vectorize = FV.funcs_vectorize in
  let calculate = calculate_mapping_distance in
  let preproc = (fun x -> x |>  Extractor.extract_func_all |> vectorize) in
  let v_sub = preproc sub in
  let topk_lst = List.map (fun (f, sol) -> (f, sol, (preproc sol))) solutions |>
                 List.map (fun (f ,sol, v_sol) -> (f, sol, (calculate v_sub v_sol))) |>
                 List.sort (fun (_,_,(_,dist)) (_,_,(_,dist')) -> compare dist dist') |>
                 BatList.take topk in
  topk_lst

let search_solutions_by_func_posvec_match : int -> prog (string * prog) list -> (string * prog * ((string * string) list * float)) list
= fun topk sub solutions ->
  let vectorize = PV.funcs_vectorize in
  let calculate = calculate_mapping_distance in
  let preproc = (fun x -> x |>  Extractor.extract_func_all |> vectorize) in
  let v_sub = preproc sub in
  let topk_lst = List.map (fun (f, sol) -> (f, sol, (preproc sol))) solutions |>
                 List.map (fun (f ,sol, v_sol) -> (f, sol, (calculate v_sub v_sol))) |>
                 List.sort (fun (_,_,(_,dist)) (_,_,(_,dist')) -> compare dist dist') |>
                 BatList.take topk in
  topk_lst
    
let search_solutions = search_solutions_by_program_match
let search_solutions2 = search_solutions_by_function_match

