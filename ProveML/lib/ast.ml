type expression =
        | Identifier of string
        | Application of expression * expression

type equation =
        | Equality of expression * expression

type declaration =
        | Arg of string * string
        | Let of string * declaration * expression
        | Rec of expression * declaration
        | Type of expression