include Ast
module Parser = Parser
module Lexer = Lexer

let rec string_of_expression e =
        match e with
        | Identifier nm -> nm
        | Application (e1, e2) -> (string_of_expression e1) ^ " " ^ (string_of_expression_with_parens e2)
        | Equals (e1, e2) -> (string_of_expression e1) ^ " = " ^ (string_of_expression e2)
and string_of_expression_with_parens e =
        match e with
        | Equals _ -> (string_of_expression e)
        | Identifier nm -> nm
        | Application _ -> "(" ^ (string_of_expression e) ^ ")"
