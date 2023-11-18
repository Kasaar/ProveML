{
open Parser
exception SyntaxError of string
}

let newline = '\r' | '\n' | "\r\n"

rule token = parse
| newline { Lexing.new_line lexbuf; token lexbuf }
| [' ' '\t'] { token lexbuf }
| "//" [^ '\n' '\r']* { token lexbuf }
| ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']+ as word { IDENT(word) }
| "(*" { comment lexbuf }
| '(' { LPAREN }
| ')' { RPAREN }
| _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
| eof { EOF }
and comment = parse
| "*)" { token lexbuf }
| newline { Lexing.new_line lexbuf ; token lexbuf }
| _ { comment lexbuf }
