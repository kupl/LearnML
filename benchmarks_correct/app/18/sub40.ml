let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> (appaux l2 [])  @ (appaux (appaux l1 []) l2) 
  
(*  and make_uniq : 'a list ->  'a list*)
(*  = fun l2 ->*)
(*    match l2 with*)
(*    | []-> []*)
(*    | h::t-> if (is_include h t) then make_uniq t else [h] @ make_uniq t*)
      
  and is_include: 'a -> 'a list -> bool
    = fun a li ->
    	match li with
    	|[]-> false
    	|h::t -> if a = h then true else (is_include a t)
    	
  and appaux : 'a list -> 'a list -> 'a list
    = fun l1 l2 ->	(appaux_check l1 l2 [])
    					
  and appaux_check : 'a list -> 'a list -> 'a list -> 'a list
  = fun l1 l2 chk ->
    match l1 with
    		|[]->[]
    		|h::t -> if (is_include h (l2@chk)) = true 
    					then (appaux_check t l2 chk)
    					else [h] @ (appaux_check t l2 (chk@[h]) )
  
;;
(* app [4;5;6;7] [1;1;1;1;3;2;3;4] ;;*)
(* app  [4;5;6;7] [1;2;3;4] ;;*)





