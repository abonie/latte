module Parsing (
    module Parsing.AbsLatte,
    --module Parsing.ParLatte,
    --module Parsing.LexLatte,
    module Parsing.ErrM,
    parse
)
where
import Parsing.AbsLatte
import Parsing.ParLatte (pProgram)
import Parsing.LexLatte (tokens)
import Parsing.ErrM


parse :: String -> Err PProgram
parse = pProgram . tokens
