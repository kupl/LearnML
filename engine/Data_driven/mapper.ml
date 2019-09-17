module type Initvec = sig
  type t
  val init_vector : (string * t) list
end 

module Make_genmap(Init : Initvec) = struct 
  type t = (string * vec) list
  and vec = Init.t list
  and perms = (string * Init.t list * string * Init.t list) list list
  
  let ins_all_positions x l = 
    let rec aux prev acc = function
    | [] -> (prev @ [x]) :: acc |> List.rev
    | hd::tl as l -> aux (prev @ [hd])((prev @ [x] @ l) :: acc) tl
  in aux [] [] l

  let rec permutations = function
  | [] -> []
  | x::[] -> [[x]]
  | x::xs -> List.fold_left (fun acc p -> acc @ ins_all_positions x p) [] (permutations xs)
  
  let padding : t -> t -> t * t
  = fun ts1 ts2 ->
    let len1 = List.length ts1 in
    let len2 = List.length ts2 in
    let empty_padding = List.map (fun (k,v) -> v) Init.init_vector in
    if len1 = len2 then ts1,ts2 
    else if len1 < len2 then
      let rec iter : t -> int -> t
      = fun acc count -> 
        if count <> (len2-len1) then 
          iter (("___padding"^string_of_int(count+1), empty_padding)::acc) 
          (count+1)
        else acc in 
          let ts1 = iter ts1 0 in ts1,ts2
    else 
      let rec iter : t -> int -> t
      = fun acc count ->
        if count <> (len1-len2) then 
          iter (("___padding"^string_of_int(count+1), empty_padding)::acc) 
          (count+1) 
        else acc in 
          let ts2 = iter ts2 0 in ts1,ts2

  let gen_mapping : t -> t -> perms
  = fun ts1 ts2 ->
    let len1 = List.length ts1 in
    let len2 = List.length ts2 in
    if len1 = len2 then 
      let perms = permutations ts2 in 
      List.fold_left (fun acc y -> 
        (List.map2 (fun (s,t) (s',t') -> (s,t,s',t')) ts1 y)::acc) [] perms
    else 
      raise (Failure "func map length must be same")
end
