import Loop
import Text.ParserCombinators.Parsec

data WhileProgram = One LoopProgram
                  | While WhileProgram
                  | More WhileProgram WhileProgram



whileScheme :: Parser WhileProgram
whileScheme = wsfnb $ do 
    wsfnb $ string "while"
    var <- variable
    wsfnb $ string "!="
    wsfnb $ char '0'
    inner <- while
    wsfnb $ string "end"
    return (While inner)


