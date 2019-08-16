open Lang
open Util
open Type

exception NotImplemented
exception Invalid_Expression_Construct
exception Emptyshouldnotbehere
exception Update_Failure

module N = struct
    (*ast flattening*)
    type node = |Node of string * (node list)
                (*expression node has label*)
                |LNode of label * node 
                |Id of string
                |Const of int
                |String of string 
                |Bool of bool 
                |Empty 

    let rec_to_node : bool -> node
    = fun b ->
      match b with
      | true -> Node ("Rec", [])
      | false -> Node ("No Rec", [])

    let rec pat_to_node : pat -> node
    = fun x ->
      match x with
      | PUnit -> Node ("PUnit",[])
      | PUnder -> Node ("PUnder",[])
      | PInt x -> Node ("PInt",[Const x])
      | PBool b -> Node ("PBool",[Bool b])
      | PVar x -> Node ("PVar", [Id x])
      | PList lst -> Node ("PList", List.map pat_to_node lst)
      | PCons lst -> Node ("PCons", List.map pat_to_node lst) 
      | PTuple lst -> Node ("PTuple", List.map pat_to_node lst) 
      | Pats lst -> Node ("Pats", List.map pat_to_node lst) 
      | PCtor (x,lst) -> Node ("PCtor", (Id x)::(List.map pat_to_node lst))
    
    let rec type_to_node : typ -> node
    = fun t ->
      match t with
      | TUnit -> Node ("TUnit",[])
      | TInt -> Node ("TInt",[])
      | TString -> Node ("TString",[])
      | TBool -> Node ("TBool",[])
      | TBase id -> Node ("TBase",[Id id])
      | TList t -> Node ("TList", [type_to_node t])
      | TTuple l -> if (l=[]) then type_to_node TUnit
                              else Node ("TTuple", List.map type_to_node l)
      | TArr (t1,t2) -> Node ("TArr", [type_to_node t1; type_to_node t2])
      | TVar x -> Node ("TVar",[Id x])
      | TCtor (x,tl) -> Node ("TCtor", (type_to_node x)::(List.map type_to_node tl))
      | TExn -> Node ("TExn",[])

    let rec let_to_node : let_bind -> node
    = fun bind ->
      match bind with 
      | BindUnder -> Node ("BindUnder", [])
      | BindOne x -> Node ("BindOne", [Id x])
      | BindTuple xs -> Node ("BindTuple", List.map let_to_node xs)

    let rec arg_to_node : arg -> node
    = fun arg ->
      match arg with
      | ArgUnder typ -> Node ("ArgUnder", [type_to_node typ])
      | ArgOne (x,typ) -> Node ("ArgOne", [Id x;type_to_node typ])
      | ArgTuple xs -> Node ("ArgTuple", List.map arg_to_node xs)

    let rec exp_to_node : lexp -> node 
    = fun (l,exp) ->
      let f : exp -> node
      = fun exp ->
        match exp with
        | EUnit -> Node ("EUnit", [])
        | Const n -> Const n
        | String id-> String id
        | TRUE -> Bool true
        | FALSE -> Bool false
        | EList lst -> Node ("EList", List.map exp_to_node lst)
        | EVar x -> Node ("EVar", [Id x]) 
        | ECtor (x,lst) -> Node ("ECtor", Id (x)::(List.map exp_to_node lst))
        | ETuple lst -> Node ("ETuple", List.map exp_to_node lst)
        | ADD (e1,e2) -> Node ("ADD", [exp_to_node e1;exp_to_node e2])
        | SUB (e1,e2) -> Node ("SUB", [exp_to_node e1;exp_to_node e2])
        | MUL (e1,e2) -> Node ("MUL", [exp_to_node e1;exp_to_node e2])
        | DIV (e1,e2) -> Node ("DIV", [exp_to_node e1;exp_to_node e2])
        | MOD (e1,e2) -> Node ("MOD", [exp_to_node e1;exp_to_node e2])
        | MINUS e -> Node ("MINUS", [exp_to_node e])
        | OR (e1,e2) -> Node ("OR", [exp_to_node e1;exp_to_node e2])
        | AND (e1,e2) -> Node ("AND", [exp_to_node e1;exp_to_node e2])
        | LESS (e1,e2) -> Node ("LESS", [exp_to_node e1;exp_to_node e2])
        | LARGER (e1,e2) -> Node ("LARGER", [exp_to_node e1;exp_to_node e2])
        | EQUAL  (e1,e2) -> Node ("EQUAL", [exp_to_node e1;exp_to_node e2])
        | NOTEQ (e1,e2) -> Node ("NOTEQ", [exp_to_node e1;exp_to_node e2])
        | LESSEQ (e1,e2) -> Node ("LESSEQ", [exp_to_node e1;exp_to_node e2])
        | LARGEREQ (e1,e2) -> Node ("LARGEREQ", [exp_to_node e1;exp_to_node e2])
        | AT (e1,e2) -> Node ("AT", [exp_to_node e1;exp_to_node e2])
        | DOUBLECOLON (e1,e2) -> Node ("DOUBLECOLON", [exp_to_node e1;exp_to_node e2])
        | STRCON (e1,e2) -> Node ("STRCON", [exp_to_node e1;exp_to_node e2])
        | NOT e -> Node ("NOT", [exp_to_node e])
        | EApp (e1,e2) -> Node ("EApp", [exp_to_node e1;exp_to_node e2])
        | EFun (arg,e) -> Node ("EFun", [arg_to_node arg;exp_to_node e])
        | IF (e1,e2,e3) -> Node ("IF", [exp_to_node e1;exp_to_node e2;exp_to_node e3;])
        | ELet (f,is_rec,args,typ,exp,exp2) ->
                Node ("ELet", [binding_to_node (f,is_rec,args,typ,exp); exp_to_node exp2])
        | EBlock (is_rec, elst, e) -> 
                Node ("EBlock", (rec_to_node is_rec)::
                                (List.map binding_to_node elst)@[exp_to_node e])
        | EMatch (e,lst) -> 
          let rec aux = fun lst -> 
          begin
            match lst with 
            |[] -> []
            |(p,lexp)::tl -> (pat_to_node p)::(exp_to_node lexp)::(aux tl) 
          end in
          Node ("EMatch", (exp_to_node e)::(aux lst))
        | Raise e -> Node ("Raise", [])
        | _ -> raise Invalid_Expression_Construct in 
      let node = f exp in
      LNode (l, node)

    and binding_to_node : binding -> node
    = fun (f,is_rec,args,typ,exp) ->
        Node ("Binding", [let_to_node f; rec_to_node is_rec]@(List.map arg_to_node args)@[type_to_node typ; exp_to_node exp])

    let decl_to_node : decl -> node
    = fun decl -> 
      match decl with
      | DExcept _ -> Empty
      | DEqn _ -> Empty
      | DData _ -> Empty
      | DLet bind_tuple -> Node ("DLet", [(binding_to_node bind_tuple)])
      | DBlock (is_rec,bind_tuples) -> Node ("DBlock", rec_to_node is_rec :: (List.map binding_to_node bind_tuples))
      | TBlock _ -> Empty

    type table = (string,int) BatHashtbl.t

    let update : table -> string -> unit
    = fun tbl s ->
      let cur = 
        try BatHashtbl.find tbl s 
        with Not_found -> raise Update_Failure in
      BatHashtbl.replace tbl s (cur+1)

    (*
    let update2 : table -> string -> unit
    = fun tbl s ->
      if BatHashtbl.mem tbl s then update tbl s
      else BatHashtbl.replace tbl s 0 
    *)

    let rec traverse : table -> node -> unit
    = fun tbl node ->
      match node with
      | LNode (l,n) -> traverse tbl n
      | Node (s,lst) -> update tbl s;
        List.iter (traverse tbl) lst
      | Id id -> update tbl "Id"
      | Const n -> update tbl "Const_Int"
      | String st -> update tbl "Const_String"
      | Bool b -> if b then update tbl "Const_True"
                       else update tbl "Const_False"
      (*
      | Id id -> update2 tbl ("Id%"^id)
      | Const n -> update2 tbl ("Const%"^(string_of_int n))
      | String st -> update2 tbl ("String%"^st)
      | Bool b -> update2 tbl ("Bool%"^(string_of_bool b))
      *)
      | Empty -> raise Emptyshouldnotbehere
         

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
        ("DBlock",0); ("Id",0); ("Const_Int",0); ("Const_String",0); ("Const_True",0); 
        ("Const_False",0);
    ]

    let init_tbl : unit -> table
    = fun x -> 
      let tbl = BatHashtbl.create 100 in
      let rec iter : table -> (string * int) list -> table 
      = fun tbl lst ->
        match lst with
        | [] -> tbl
        | (hd,count)::tl -> BatHashtbl.replace tbl hd count; iter tbl tl
      in iter tbl init_vector 
end


    type t = int list

    (*
    let to_string: t -> string
    = fun t ->
      let int_vec = List.map (fun (k,v) -> v) t in
      List.fold_left (fun acc e -> acc^(string_of_int e)^" ") "" int_vec  
    *)

    (*
    let print_list : t -> unit
    = fun lst ->
      (*let cnt = List.map (fun (_,x) -> x) lst in*)
      let rec traverse = fun x -> 
        match x with
        | [] -> print_endline "end of list"
        | (s,c)::tl -> print_endline (s^" "^string_of_int c); traverse tl in
      traverse lst
    *)

    let ast_flatten : prog -> N.node list
    = fun prog -> 
      let flat = List.map N.decl_to_node prog in
      List.filter (fun x -> x <> N.Empty) flat 

    let node_vectorize: N.node -> t
    = fun node -> 
      let table = N.init_tbl () in N.traverse table node;
      let fold h = BatHashtbl.fold (fun k v acc -> (k,v) :: acc) h [] in
      let sorted = List.sort compare (fold table) in 
      List.map (fun (k,v) -> v ) sorted 

    let prog_vectorize: prog -> t
    = fun prog -> 
      let ast = ast_flatten prog in
      let table = N.init_tbl () in List.iter (N.traverse table) ast;
      let fold h = BatHashtbl.fold (fun k v acc -> (k,v) :: acc) h [] in
      let sorted = List.sort compare (fold table) in 
      List.map (fun (k,v) -> v) sorted 

    let rec funcs_vectorize: (string * lexp) list -> (string * t) list
    = fun lst -> 
      match lst with
      | [] -> []
      | (f,lexp)::tl -> 
        let vec = lexp |> N.exp_to_node |> node_vectorize in
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

    let ins_all_positions x l =
      let rec aux prev acc = function
      | [] -> (prev @ [x]) :: acc |> List.rev
      | hd::tl as l -> aux (prev @ [hd])((prev @ [x] @ l) :: acc) tl
    in aux [] [] l

    let rec permutations = function
    | [] -> []
    | x::[] -> [[x]]
    | x::xs -> List.fold_left (fun acc p -> acc @ ins_all_positions x p) [] (permutations xs)

    let rec combinations = function
    | [] -> [[]]
    | h::t -> 
      let cs = combinations t in
      List.map (fun x -> h::x) cs @ cs 

    let partial_combinations : 'a list -> int -> 'a list list
    = fun l len ->
      let combs = combinations l in
      List.filter (fun x -> List.length x = len) combs

    let gen_mapping : (string*t) list -> (string*t) list -> (string * t * string * t) list list
    = fun ts1 ts2 ->
        let len1 = List.length ts1 in
        let len2 = List.length ts2 in

        if len1 = len2 then 
          let perms = permutations ts2 in 
          List.fold_left (fun acc y -> 
            (List.map2 (fun (s,t) (s',t') -> (s,t,s',t')) ts1 y)::acc) [] perms
        (*num of func is different*)
        else if len1 < len2 then 
          let combs = partial_combinations ts2 len1 in
          let perms = List.map (fun x -> permutations x) combs |> List.flatten in
          List.fold_left (fun acc y ->
            (List.map2 (fun (s,t) (s',t') -> (s,t,s',t')) ts1 y)::acc) [] perms
        (*len1 > len2*)
        else []


    (*
    let funcs_calculate_distance : (string*t) list -> (string*t) list -> float
    = fun ts1 ts2 -> 
    *)
          
    let search_solutions_by_program_match : int -> prog -> (string * prog) list -> (string * prog * float) list
    = fun topk sub solutions ->
      let vectorize = prog_vectorize in
      let calculate = calculate_distance in
      let v_sub = vectorize sub in
      let v_solutions = List.map (fun (f, sol) -> (f, sol, (vectorize sol))) solutions in
      let dists = List.map (fun (f, sol, v_sol) -> (f, sol, (calculate v_sub v_sol))) v_solutions in
      let sorted = List.sort (fun (_,_,dist) (_,_,dist') -> compare dist dist') dists in

      (*inner function pick_cand use free variable 'topk' *)
      let rec pick_cand acc count cand = 
        match cand with
        | [] -> acc
        | h::t -> if (count <= topk) then pick_cand (acc@[h]) (count+1) t else acc in

      let topk_lst = pick_cand [] 1 sorted in
      topk_lst

    let search_solutions_by_function_match : int -> prog -> (string * prog) list -> (string * prog * float) list
    = fun topk sub solutions ->
      let vectorize = prog_vectorize in
      let calculate = calculate_distance in
      let v_sub = vectorize sub in
      let v_solutions = List.map (fun (f, sol) -> (f, sol, (vectorize sol))) solutions in
      let dists = List.map (fun (f, sol, v_sol) -> (f, sol, (calculate v_sub v_sol))) v_solutions in
      let sorted = List.sort (fun (_,_,dist) (_,_,dist') -> compare dist dist') dists in 

      let rec pick_cand acc count cand =
        match cand with
        | [] -> acc
        | h::t -> if (count <= topk) then pick_cand (acc@[h]) (count+1) t else acc in

      let topk_lst = pick_cand [] 1 sorted in
      topk_lst

    let search_solutions = search_solutions_by_program_match 

