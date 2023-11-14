type expression =
        | Indentifier of string
        | Application of expression * expression
