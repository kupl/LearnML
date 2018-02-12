open Lang
open Util

type feature_vector = int array

let init_feature () : feature_vector = Array.make 35 0

let inc index feature = feature.(index) <- ((feature.(index)) + 1)

type feature = {
	occam : feature_vector; (* Occam's razor *)
	appearance : feature_vector; (* the number of appearnce of the expressions *)
}

let empty_feature = {
	occam = init_feature ();
	appearance = init_feature ();
}

let occam_feature feat = 
	{ feat with occam = [|50;15;15;15;15;15;15;15;15;15;15;15;15;15;15;15;15;15;15;15;15;10;10;50;50;15;40;30;40;15;15;10;5;30;30|] }

let extract_appearance feat = 
	function
	| EUnit -> inc 0 feat; feat
  | Const _ -> inc 1 feat; feat
  | TRUE ->  inc 2 feat; feat
  | FALSE ->  inc 3 feat; feat
  | String _ -> inc 4 feat; feat
  | ADD (e1,e2) -> inc 5 feat; (extract_appearance feat e1 |> extract_appearance) e2
  | SUB (e1,e2) -> inc 6 feat; (extract_appearance feat e1 |> extract_appearance) e2
  | MUL (e1,e2) -> inc 7 feat; (extract_appearance feat e1 |> extract_appearance) e2
  | DIV (e1,e2) -> inc 8 feat; (extract_appearance feat e1 |> extract_appearance) e2
  | MOD (e1,e2) -> inc 9 feat; (extract_appearance feat e1 |> extract_appearance) e2
  | OR (e1,e2) ->  inc 10 feat; (extract_appearance feat e1 |> extract_appearance) e2
  | AND (e1,e2) -> inc 11 feat; (extract_appearance feat e1 |> extract_appearance) e2
  | LESS (e1,e2) -> inc 12 feat; (extract_appearance feat e1 |> extract_appearance) e2
  | LARGER (e1,e2) -> inc 13 feat; (extract_appearance feat e1 |> extract_appearance) e2
  | EQUAL (e1,e2) -> inc 14 feat; (extract_appearance feat e1 |> extract_appearance) e2
  | NOTEQ (e1,e2) -> inc 15 feat; (extract_appearance feat e1 |> extract_appearance) e2
  | LESSEQ (e1,e2) -> inc 16 feat; (extract_appearance feat e1 |> extract_appearance) e2
  | LARGEREQ (e1,e2) -> inc 17 feat; (extract_appearance feat e1 |> extract_appearance) e2
  | STRCON (e1,e2) -> inc 18 feat; (extract_appearance feat e1 |> extract_appearance) e2
  | MINUS e1 -> inc 19 feat; extract_appearance feat e1
  | NOT e1 -> inc 20 feat; extract_appearance feat e1
  | EVar x -> inc 21 feat; feat
  | EApp (e1,e2) -> inc 22 feat; (extract_appearance feat e1 |> extract_appearance) e2
  | ELet (_,_,_,_,e1,e2) -> inc 23 feat; (extract_appearance feat e1 |> extract_appearance) e2
  | EBlock (_, es, e2) -> inc 24 feat; list_fold (fun (_, _, _, _, e) feat -> extract_appearance feat e) es (extract_appearance feat e2)
	| ECtor (_,l) -> inc 25 feat; list_fold (fun e feat-> extract_appearance e feat) l feat
  | EMatch (e1,bl) -> inc 26 feat; list_fold (fun (_,e) feat -> extract_appearance e feat) bl (extract_appearance feat e1)
  | EFun (arg,e) -> inc 27 feat; extract_appearance feat e
  | IF (e1,e2,e3) -> inc 28 feat; (((extract_appearance feat e1 |> extract_appearance) e2) |> extract_appearance) e3
  | AT (e1,e2) -> inc 29 feat; (extract_appearance feat e1 |> extract_appearance) e2
  | DOUBLECOLON (e1,e2) -> inc 30 feat; (extract_appearance feat e1 |> extract_appearance) e2
  | EList l -> inc 31 feat; list_fold (fun e feat-> extract_appearance e feat) l feat
  | ETuple l-> inc 32 feat; list_fold (fun e feat-> extract_appearance e feat) l feat
  | Hole n-> inc 33 feat; feat
  | Raise e -> inc 34 feat; extract_appearance feat e

let extract_appearance_decl : decl -> feature_vector -> feature_vector
= fun decl feat ->
	match decl with
	| DLet (x,is_rec,args,typ,exp) -> extract_appearance feat exp
	| DBlock (is_rec, bindings) -> list_fold (fun (_,_,_,_,e) feat -> extract_appearance e feat) bindings feat
	| _ -> feat

let extract_appearance_pgm : feature_vector -> prog -> feature_vector
= fun feat pgm -> list_fold extract_appearance_decl pgm feat

let appearance_feature pgm feat = 
	{ feat with appearance = (extract_appearance_pgm (init_feature ()) pgm) }
