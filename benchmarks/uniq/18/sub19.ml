let rec uniq : 'a list -> 'a list
= fun lst -> 
  let rec isinlist n remainlst =
    match remainlst with
    | [] -> false
    | h::tl ->
        begin
          if h=n then true
          else isinlist n tl
        end
  in
  let rec loop afterlst beforelst =
    match beforelst with
    | [] -> afterlst
    | h::tl ->
        begin
          if isinlist h afterlst then loop afterlst tl
          else loop (h::afterlst) beforelst
        end
  in
  List.rev (loop [] lst);;
  
