{
open Lexing
open Parser

exception Lexer_error of string

let reserved_words : (string * Parser.token) list =
  [ ("fun", FUN)
  ; ("function", FUNCTION)
  ; ("match", MATCH)
  ; ("with", WITH)
  ; ("type", TYPE)
  ; ("of", OF)
  ; ("let", LET)
  ; ("in", IN)
  ; ("rec", REC)
  ; ("if",IF)
  ; ("then",THEN)
  ; ("else",ELSE)
  ; ("not",NOT)
  ; ("true",TRUE)
  ; ("false",FALSE)
  ; ("mod", MOD)
  ; ("bool",TBool)
  ; ("int",TInt)
  ; ("list",TList)
  ; ("string", TString)
  ; ("unit", TUnit)
  ; ("begin", BEGIN)
  ; ("end", END)
  ; ("exception",EXCEPTION)
  ; ("raise",RAISE)
  ; ("and",DEFAND)
  ; ("List.hd",LISTHD)
  ; ("List.tl",LISTTL)
  ; ("List.map",LISTMAP)
  ; ("List.mem",LISTMEM)
  ; ("List.exists",LISTEXISTS)
  ; ("List.filter",LISTFILTER)
  ; ("List.append",LISTAPPEND)
  ; ("List.length",LISTLENGTH)
  ; ("List.nth",LISTNTH)
  ; ("List.rev",LISTREV)
  ; ("List.fold_left",LISTFOLDL)
  ; ("List.fold_right",LISTFOLDR)
  ; ("List.sort",LISTSORT)
  ; ("List.rev_map",LISTREVMAP)
  ; ("List.memq",LISTMEMQ)
  ; ("List.rev_append",LISTREVAPD)
  ; ("List.mapi", LISTMAPI)
  ; ("List.for_all",LISTFORALL)
  ; ("List.find",LISTFIND)
  ; ("List.assoc", LISTASSOC)
  ; ("String.concat", STRINGCONCAT)
  ; 
  ]

let symbols : (string * Parser.token) list =
  [ ("?", HOLE)
  ; ("=", EQ)
  ; ("->", ARR)
  ; ("=>", FATARR)
  ; (",", COMMA)
  ; (":", COLON)
  ; (";", SEMI)
  ; ("*", STAR)
  ; ("|", PIPE)
  ; ("(", LPAREN)
  ; (")", RPAREN)
  ; ("{", LBRACE)
  ; ("}", RBRACE)
  ; ("[", LBRACKET)
  ; ("]", RBRACKET)
  ; ("_", UNDERBAR)
  ; ("+", PLUS)
  ; ("-", MINUS)
  ; ("/", DIVIDE)
  ; ("||", OR)
  ; ("&&",AND)
  ; ("&",AND)
  ; ("<", LESS)
  ; (">", LARGER)
  ; ("<=", LESSEQ)
  ; (">=", LARGEREQ)
  ; ("!=", NOTEQ)
  ; ("<>", NOTEQ)
  ; ("@",AT)
  ; ("::",DOUBLECOLON)
  ; ("^",STRCON)
  ; ("'",IDENT)
  ; ("#S", SHOLE)
  ; ("#A", AHOLE)
  ]

let create_token lexbuf =
  let str = lexeme lexbuf in
  match Util.lookup str reserved_words with
  | None   -> LID str
  | Some t -> t

let create_symbol lexbuf =
  let str = lexeme lexbuf in
  match Util.lookup str symbols with
  | None   -> raise @@ Lexer_error ("Unexpected token: " ^ str)
  | Some t -> t

let create_external lexbuf =
  let str = lexeme lexbuf in
  match Util.lookup str reserved_words with
  | None -> raise @@ Lexer_error ("Unexpected token: "^str)
  | Some t -> t
}

let newline    = '\n' | ('\r' '\n') | '\r'
let whitespace = ['\t' ' ']
let lowercase  = ['a'-'z'] 
let uppercase  = ['A'-'Z']
let character  = uppercase | lowercase
let digit      = ['0'-'9']

rule token = parse
  | eof   { EOF }
  | digit+ { INT (int_of_string (lexeme lexbuf)) }
  | "(*" {comments 0 lexbuf}
  | '"' {STRING (create_string (Buffer.create 100) lexbuf)}
  | whitespace+ | newline+    { token lexbuf }
  | '_' (digit | character | ''' | '_')+    { create_token lexbuf }
  | lowercase (digit | character | ''' | '_')*    { create_token lexbuf }
  | uppercase (digit | character | ''' | '_')*    { UID (lexeme lexbuf) }
  | uppercase (digit | character | ''' | '_')*('.')(digit | character | ''' | '_')* {create_external lexbuf}
  | '?' | "|>" | '=' | "->" | "=>" | '*' | ',' | ':' | ';' | '|' | '(' | ')' | '{' | '}' | '[' | ']' 
  | '_' | '+' | '-' | '/'| '%' | "||" | "&&" | "<" | ">" | "<=" | ">=" | "!=" | "<>" | "@"| "::" | "'" |"^" | "&" 
    { create_symbol lexbuf }
  | _ as c { raise @@ Lexer_error ("Unexpected character: " ^ Char.escaped c) }

and comments level = parse
  | "*)" { if level = 0 then token lexbuf
     else comments (level-1) lexbuf }
  | "(*" { comments (level+1) lexbuf}
  | [^ '\n'] { comments level lexbuf }
  | "\n" { comments level lexbuf }
  | eof  { failwith "Comments are not closed" }

and create_string buf = parse
  | '"' {Buffer.contents buf}
  | eof  { failwith "String are not closed" }
  | _ as c {
      Buffer.add_char buf c; 
      create_string buf lexbuf 
    }
