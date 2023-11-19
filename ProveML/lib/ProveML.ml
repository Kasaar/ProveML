include Ast
module Parser = Parser
module Lexer = Lexer

let rec string_of_expression e =
        match e with
        | Identifier nm -> nm
        | Application (e1, e2) -> (string_of_expression e1) ^ " " ^ (string_of_expression_with_parens e2)
and string_of_expression_with_parens e =
        match e with
        | Identifier nm -> nm
        | Application _ -> "(" ^ (string_of_expression e) ^ ")"

let string_of_equation e =
        match e with
        | Equality (e1, e2) -> "(" ^ (string_of_expression e1) ^ " = " ^ (string_of_expression e2) ^ ")"

let rec string_of_declaration d =
        match d with
        | Let (nm, args, e) -> "let (*prove*) " ^ nm ^ " " ^ (string_of_declaration args) ^ " = " ^ (string_of_equation e)
        | Arg (nm1 , nm2) -> "(" ^ nm1 ^ " : " ^ nm2 ^ ")"
        | _ -> "Not implemented in ProveML"