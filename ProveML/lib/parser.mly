%{
open Ast
%}
%token <string> IDENT
%token PROVE
%token LET
%token EQUALS
%token LPAREN
%token RPAREN
%token COLON
%token EOF
%start main
// %type <expression list> main
// %type <equation> equation
%type <declaration list> main
%%
main:
| d = list(declaration) ; EOF { d }
declaration:
| LPAREN ; nm1 = IDENT ; COLON ; nm2 = IDENT ; RPAREN { Arg (nm1, nm2) }
| LET ; PROVE ; nm = IDENT ; d = declaration ; EQUALS ; e = expression { Let (nm, d, e) }
expression:
| LPAREN ; e = expression ; RPAREN { e }
| nm = IDENT { Identifier nm }
| e1 = expression ; nm = IDENT { Application (e1,Identifier nm) }
| e1 = expression ; LPAREN ; e2 = expression ; RPAREN ; { Application (e1, e2) }



// main:
// | e = expression ; EOF { [e] }
// expression:
// | LPAREN ; e = expression ; RPAREN { e }
// | nm = IDENT { Identifier nm }
// | e1 = expression ; nm = IDENT { Application (e1,Identifier nm) }
// | e1 = expression ; LPAREN ; e2 = expression ; RPAREN ; { Application (e1, e2) }
// equation:
// | e1 = expression ; EQUALS ; e2 = expression { Equation (e1, e2) }
// | nm = IDENT ; EQUALS ; e = expression { Equation (Identifier nm, e) }
