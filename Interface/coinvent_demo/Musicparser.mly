%{open Chord%}



%token SPEC
%token THEN
%token CHORD
%token OP
%token HASABSNOTE
%token ROOT
%token HASRELNOTE
%token END
%token PRIORITY
%token OPEN
%token CLOSE
%token EOF
%token COLON
%token PERCENT
%token <string> IDENT
%token EQUALS
%token <int> INT
%token COMMA
%token COMMENT
%token EOL
%token DOT
%token NOT
%token SUC
%token SORT
%token SORTS
%token LOGIC
%token OPS
%token PRED
%token PREDS
%token GENERATED
%token FREE
%start chord1
%type <Chord.chord> chord1


%%

chord:
| SPEC IDENT EQUALS chord {$4}
| OP IDENT COLON CHORD chord {$5}
| CHORD chord {$2} 
| THEN chord {$2}
| DOT HASRELNOTE OPEN IDENT COMMA INT CLOSE chord { Cons(RelNote($6),$8) }
| DOT HASABSNOTE OPEN IDENT COMMA INT CLOSE chord { Cons(AbsNote($6),$8) }
| DOT ROOT OPEN IDENT CLOSE EQUALS INT chord {Cons(Root($7),$8) }
| DOT NOT INT EQUALS INT chord {$6}
| DOT SUC OPEN INT CLOSE EQUALS INT chord {$8}
| PERCENT OPEN IDENT CLOSE PERCENT chord {$6}
| END {Nil}
| EOF {Nil};

chord1:
| chord { $1 };
