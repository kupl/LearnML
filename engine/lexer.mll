{
open Lexing
open Parser

exception Lexer_error of string

let reserved_words : (string * Parser.token) list =
  [ ("fun", FUN)
  ; ("function", FUN)
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
  ]

let symbols : (string * Parser.token) list =
  [ ("?", HOLE)
(*; ("|>", IMPLIES) *)
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
(*
let create_string lexbuf = 
  let str = lexeme lexbuf in
  STRING str
*)
}

let newline    = '\n' | ('\r' '\n') | '\r'
let whitespace = ['\t' ' ']
let lowercase  = ['a'-'z'] | '_'
let uppercase  = ['A'-'Z']
let character  = uppercase | lowercase
let digit      = ['0'-'9']

rule token = parse
  | eof   { EOF }
  | digit+ { INT (int_of_string (lexeme lexbuf)) }
  | "(*" {comments 0 lexbuf}
  | '"' {STRING (create_string (Buffer.create 100) lexbuf)}
  | whitespace+ | newline+    { token lexbuf }
  | lowercase (digit | character | ''')*    { create_token lexbuf }
  | uppercase (digit | character | ''')*    { UID (lexeme lexbuf) }
  | '?' | "|>" | '=' | "->" | "=>" | '*' | ',' | ':' | ';' | '|' | '(' | ')' | '{' | '}' | '[' | ']' 
  | '_' | '+' | '-' | '/'| '%' | "||" | "&&" | "<" | ">" | "<=" | ">=" | "!=" | "<>" | "@"| "::" | "'" 
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
