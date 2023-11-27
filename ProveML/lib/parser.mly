%{
open Ast
%}
%token <string> IDENT
%token PROVE
%token LET
%token AXIOM
%token HINT
%token ENDCOMMENT
%token EQUALS
%token LPAREN
%token RPAREN
%token COLON
%token COMMA
%token EOF
%start main
%type <declaration list> main
%%
main:
| d = list(declaration) ; EOF { d }
declaration:
| LET ; PROVE ; nm = IDENT ; args = list(argument) ; EQUALS ; e = equation ; hint = option(hint) { Let (nm, args, e, hint) }
argument:
| nm = IDENT ; COLON ; t = IDENT { Arg (nm, t) }
| LPAREN ; arg = argument ; RPAREN { arg }
equation:
| e1 = expression ; EQUALS ; e2 = expression { Equality (e1, e2) }
| LPAREN ; e1 = expression ; EQUALS ; e2 = expression ; RPAREN { Equality (e1, e2) }
hint:
| HINT ; AXIOM ; ENDCOMMENT { Axiom }
expression:
| LPAREN ; e = expression_with_commas ; RPAREN { e }
| lhs = expression ; arg = IDENT { Application (lhs, Identifier arg) }
| lhs = expression ; LPAREN ; arg = expression_with_commas ; RPAREN { Application (lhs, arg) }
| nm = IDENT { Identifier nm }
expression_with_commas:
| e = expression { e }
| e1 = expression_with_commas ; COMMA ; e2 = expression { Application (Application (e1, Identifier ","), e2) }