{ (*header*)

open Lexing
open Nameparser

let keyword_al = [
   ("priority",PRIORITY);
   ("Chord", CHORD);
   ("op",OP);
   ("root",ROOT);
   ("spec",SPEC);
   ("then",THEN);
   ("hasAbsNote",HASABSNOTE);
   ("hasRelNote",HASRELNOTE);
   ("end",END);
   ("sort",SORT);
   ("sorts",SORTS);
   ("pred",PRED); 
   ("op",OPS);
   ("preds",PREDS);
   ("logic",LOGIC);
   ("not",NOT);
   ("suc",SUC);
   ("generated",GENERATED);
   ("generated",FREE)
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
  | "%("            {priority lexbuf;token lexbuf}
  | "op"            {rem lexbuf;token lexbuf}
  | "pred"          {rem lexbuf;token lexbuf}
  | "ops"            {rem lexbuf;token lexbuf}
  | "preds"          {rem lexbuf;token lexbuf}
  | "forall"        {logic lexbuf;token lexbuf}
  | "generated"     {logic lexbuf;token lexbuf}
  | "free"     {logic lexbuf;token lexbuf}
  | "logic"         {rem lexbuf;token lexbuf}
  | "sorts"         {rem lexbuf;token lexbuf}
  | "sort"          {rem lexbuf;token lexbuf}
  | '=' {EQUALS}
  | '('             { OPEN }
  | ')'             { CLOSE }
  | ':'             { COLON }
  | ','             { COMMA }
  | '%'             { PERCENT }
  | '.'             { rem lexbuf;token lexbuf }
  | eof             { EOF }
  | ident {let s = Lexing.lexeme lexbuf in
                          try List.assoc s keyword_al
                          with Not_found -> IDENT(s)}
  | _ {rem lexbuf;token lexbuf}

and comment = parse
  | "%%" {comment lexbuf}
  | newline {}
  | eof {}
  | _ {comment lexbuf}
and priority = parse
  | "%(" {priority lexbuf}
  | ")%" {}
  | _ {priority lexbuf}
and rem = parse
  | newline {}
  | _ {rem lexbuf}
and logic = parse
  | "%(" {comment lexbuf}
  | _ {logic lexbuf}
{}

