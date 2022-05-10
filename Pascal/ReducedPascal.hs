module ReducedPascal where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Expr
import Text.ParserCombinators.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

type Variable = String

data Expression = Equals Expression Expression
				| Different Expression Expression
				| Less Expression Expression
				| LessEq Expression Expression
				| Greater Expression Expression
				| GreaterEq Expression Expression
				| Plus Expression Expression
				| Mult Expression Expression
				| Div Expression Expression
				| Mod Expression Expression
				| Constant Int
				| Var Variable
				| ArrayIndexedAt Variable Expression
				deriving (Eq, Show)


