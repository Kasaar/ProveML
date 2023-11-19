type expression =
        | Identifier of string
        | Application of expression * expression

type equation =
        | Equality of expression * expression

type declaration =
        | Arg of expression * expression
        | Let of expression * declaration * expression
        | Rec of expression * declaration
        | Type of expression