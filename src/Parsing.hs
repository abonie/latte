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
import Errors.LatteError


parse :: String -> Either (LatteError PType) PProgram
parse source = case pProgram $ tokens source of
    Bad err -> Left $ ParserError err
    Ok p -> Right p
