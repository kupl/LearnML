open Lang
open Util
open Type
open Node
open Mapper

exception Emptyshouldnotbehere
exception Table_Update_Failure

type t = int list

let init_vector = [
  ("Rec",0); ("No Rec",0); ("PUnit",0); ("PUnder",0); ("PInt",0);
  ("PBool",0); ("PVar",0); ("PList",0); ("PCons",0); ("PTuple",0);
  ("Pats",0); ("PCtor",0); ("TUnit",0); ("TInt",0); ("TString",0);
  ("TBool",0);("TBase",0);("TList",0);("TTuple",0);("TArr",0);
  ("TVar",0);("TCtor",0);("TExn",0);("BindUnder",0);("BindOne",0);
  ("BindTuple",0);("ArgUnder",0);("ArgOne",0);("ArgTuple",0);
  ("EUnit",0); ("EList",0); ("EVar",0); ("ECtor",0); ("ETuple",0); ("ADD",0);
  ("SUB",0); ("MUL",0); ("DIV",0); ("MOD",0); ("MINUS",0);
  ("OR",0); ("AND",0); ("LESS",0); ("LARGER",0); ("EQUAL",0);
  ("NOTEQ",0); ("LESSEQ",0); ("LARGEREQ",0); ("AT",0); ("DOUBLECOLON",0);
  ("STRCON",0); ("NOT",0); ("EApp",0); ("EFun",0); ("IF",0); ("ELet",0);
  ("EBlock",0); ("EMatch",0); ("Raise",0); ("Binding",0); ("DLet",0); 
  ("DBlock",0); ("Id",0); ("Const_Int",0); ("Const_String",0); ("Const_Bool",0);
]

let init_tbl = BatHashtbl.of_list init_vector 
let gen_tbl () = BatHashtbl.copy init_tbl

module Freq_map =
  Make_genmap(struct 
    type t = int
    let init_vector = init_vector
  end)

let update : (string, int) BatHashtbl.t -> string -> unit
= fun tbl s -> 
  let cur = 
    try BatHashtbl.find tbl s 
    with Not_found -> raise Table_Update_Failure in
  BatHashtbl.replace tbl s (cur+1)

let rec traverse : (string, int) BatHashtbl.t -> node -> unit
= fun tbl node ->
  match node with
  | LNode (l,n) -> traverse tbl n
  | Node (h,s,lst) -> update tbl s;
    List.iter (traverse tbl) lst
  | Leaf -> ()
  | Empty -> raise Emptyshouldnotbehere

let node_vectorize: node -> t
= fun node -> 
  let table = gen_tbl () in traverse table node;
  BatHashtbl.to_list table |> List.sort compare |> List.map (fun (k,v) -> v)

let prog_vectorize: prog -> t
= fun prog -> 
  let ast = List.map decl_to_node prog |>
            List.filter (fun x -> x <> Empty) in
  let table = gen_tbl () in 
  List.iter (traverse table) ast;
  BatHashtbl.to_list table |> List.sort compare |> List.map (fun (k,v) -> v) 

let rec funcs_vectorize: (string * lexp) list -> (string * t) list
= fun lst -> 
  match lst with
  | [] -> []
  | (f, lexp)::tl -> 
    let vec = lexp |> exp_to_node |> node_vectorize in
    (f, vec) :: (funcs_vectorize tl) 

let calculate_distance : t -> t -> float
= fun t1 t2 ->
  let rec sum : int list -> int list -> int
  = fun v1 v2 -> 
    match v1,v2 with
    | [],[] -> 0
    | h::t, h'::t' -> (h-h')*(h-h') + sum t t' 
    | _ -> raise (Failure "vector should have same dimension") in
  sqrt (float_of_int(sum t1 t2))

let rec gen_score_map : (string * t) list -> (string * t) list -> ((string * string) * float) list 
= fun ts1 ts2 ->
  match ts1 with
  | [] -> []
  | (s,v)::t ->
    let with_s = List.map (fun (s',v') -> let dist = calculate_distance v v' in 
    (s,s'), dist) ts2 in with_s @ gen_score_map t ts2

let calculate_mapping_distance : (string*t) list -> (string*t) list -> (string * string) list * float 
= fun ts1 ts2 ->
  if (List.length ts1) = (List.length ts2) then 
  begin
    let ts1,ts2 = Freq_map.padding ts1 ts2 in
    let all_func_mapping = Freq_map.gen_mapping ts1 ts2 in
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
  end 
  else ([],max_float)
    
let search_solutions_by_program_match : int -> prog -> (string * prog) list -> (string * prog * float) list 
= fun topk sub solutions ->
  let vectorize = prog_vectorize in 
  let calculate = calculate_distance in
  let v_sub = vectorize sub in
  let topk_lst = List.map (fun (f, sol) -> (f, sol, (vectorize sol))) solutions |> 
                 List.map (fun (f, sol, v_sol) -> (f, sol, (calculate v_sub v_sol))) |>
                 List.sort (fun (_,_,dist) (_,_,dist') -> compare dist dist') |>
                 BatList.take topk in
  topk_lst
    
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
    
let search = search_solutions_by_program_match
let search2 = search_solutions_by_function_match

