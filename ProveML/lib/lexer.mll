{
open Parser
exception SyntaxError of string
}

let newline = '\r' | '\n' | "\r\n"

rule token = parse
| newline { Lexing.new_line lexbuf; token lexbuf }
| [' ' '\t'] { token lexbuf }
| "//" [^ '\n' '\r']* { token lexbuf }
| ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']+ as word 
        { match word with
                | "(*prove*)" -> PROVE
                | "let" -> LET
                | _ -> IDENT(word) }
| "(*" { comment 0 lexbuf }
| '(' { LPAREN }
| ')' { RPAREN }
| _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
| eof { EOF }
and comment level = parse
| "*)" { if level = 0 then token lexbuf
        else comment (level - 1) lexbuf }
| "(*" { comment (level + 1) lexbuf }
| eof { raise (SyntaxError "Unclosed comment") }
| _ { comment level lexbuf }
