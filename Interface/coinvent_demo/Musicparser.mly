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
%token DOT
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
| PERCENT PRIORITY OPEN INT CLOSE PERCENT chord {$7}
| END {Nil};

chord1:
| chord EOF { $1 };
