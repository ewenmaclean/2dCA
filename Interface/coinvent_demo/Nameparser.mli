type token =
  | SPEC
  | THEN
  | CHORD
  | OP
  | HASABSNOTE
  | ROOT
  | HASRELNOTE
  | END
  | PRIORITY
  | OPEN
  | CLOSE
  | EOF
  | COLON
  | PERCENT
  | IDENT of (string)
  | EQUALS
  | INT of (int)
  | COMMA
  | COMMENT
  | EOL
  | DOT
  | NOT
  | SUC
  | SORT
  | SORTS
  | LOGIC
  | OPS
  | PRED
  | PREDS
  | GENERATED
  | FREE

val chord1 :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string
