{ (*header*)

open Lexing
open Musicparser

let keyword_al = [
   ("priority",PRIORITY);
   ("Chord", CHORD);
   ("op",OP);
   ("root",ROOT);
   ("spec",SPEC);
   ("then",THEN);
   ("hasAbsNote",HASABSNOTE);
   ("hasRelNote",HASRELNOTE);
   ("end",END)
]
}
let digit = ['0'-'9']
let letter = ['?' 'A'-'Z' '_' 'a'-'z']
let alphanum = digit | letter
let ident = letter alphanum*
let newline = ('\010' | '\013' | "\013\010")

rule token = parse
  | [' ' '\n' '\t'] { token lexbuf }
  | "x1" {INT(11)}
  | ['0'-'9' 'x']+ as s { if s="x" then INT(10) else INT(int_of_string s) }
  | "%%"            {comment lexbuf;token lexbuf}
  | '=' {EQUALS}
  | '('             { OPEN }
  | ')'             { CLOSE }
  | ':'             { COLON }
  | ','             { COMMA }
  | '%'             { PERCENT }
  | '.'             { DOT }
  | eof             { EOF }
  | ident {let s = Lexing.lexeme lexbuf in
                          try List.assoc s keyword_al
                          with Not_found -> IDENT(s)}

and comment = parse
  | "%%" {comment lexbuf}
  | newline {}
  | eof {}
  | _ {comment lexbuf}
{}

