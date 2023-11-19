%{
open Ast
%}
%token <string> IDENT
%token PROVE
%token LET
%token LPAREN
%token RPAREN
%token EQUALS
%token EOF
%start main
%type <Ast.expression list> main
%%
main:
| e = expression ; EOF { [e] }
expression:
| LPAREN ; e = expression ; RPAREN { e }
| nm = IDENT { Identifier nm }
| e1 = expression ; nm = IDENT { Application (e1,Identifier nm) }
| e1 = expression ; LPAREN ; e2 = expression ; RPAREN ; { Application (e1, e2) }
| e1 = expression ; EQUALS ; e2 = expression { Equals(e1, e2) }
