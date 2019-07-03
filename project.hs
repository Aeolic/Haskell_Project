import Control.Applicative
import Control.Monad
import Data.Function
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad.State
import qualified Data.Map as M
import ParserCon

-- ^ Parsing

type Id = Char
type NumI = Integer

data System =  System [Header] [Rule]
     deriving (Show, Eq)
data Header = Axiom Id | Angle Integer
  deriving (Show, Eq)
data Rule = Rule Id [Atom]
  deriving (Show, Eq)
data Atom = Symb Symbol | Ide Char
  deriving (Show, Eq)
data Symbol = DrawF | DrawB | MoveF| MoveB | PlusR | MinusR | Push | Pop
  deriving(Show, Eq)


parseString :: String -> Maybe System
parseString s = do
            l <- lexer s
            parse parser l

parser :: Parser Token System
parser = System <$> many (parseHeader) <*> many1 (parseRule)


parseAtom :: Parser Token Atom
parseAtom = try (\token -> case token of 
                              TSymb smb -> Just $ Symb smb 
                              TId id -> Just $ Ide id
                              _ -> Nothing) --could split into 2, might be necessary for rule parser

parseId :: Parser Token Id
parseId = try (\token -> case token of 
                              TId id -> Just id
                              _ -> Nothing)

parseRule :: Parser Token Rule --(Id parser?, many1 or many?)
parseRule = Rule <$> (parseId <* lit TAsgn) <*> (many1 parseAtom) 

parseHeader :: Parser Token Header 
parseHeader = Axiom <$> (lit TAxiom *> parseId) 
              <|> Angle <$> (lit TAngle *> parseNum)

parseNum :: Parser Token NumI
parseNum = try (\token -> case token of 
                              TNum x-> Just x
                              _ -> Nothing)

-- ^ Lexing
-- Use this lexer to tokenize the input before parsing
data Token = TAsgn -- '->'
           | TNum Integer
           | TId Id
           | TSymb Symbol
           | TNewLine
           | TAxiom
           | TAngle
  deriving (Eq, Show)

lexer :: String -> Maybe [Token]
lexer = parse $ many1 (skipSpace *> p_tok) <* skipSpace

skipSpace = many (satisfy isSpace)
p_tok =
  t_newline
  <|> t_num
  <|> t_asgn
  <|> t_symbol
  <|> t_id
  <|> t_alnum


t_symbol = fmap TSymb $
         DrawF <$ lit 'F'
        <|> DrawB <$ lit 'B'
        <|> MoveF <$ lit 'f'
        <|> MoveB <$ lit 'b'
        <|> PlusR <$ lit '+'
        <|> MinusR <$ lit '-'
        <|> Push <$ lit '['
        <|> Pop <$ lit ']'


t_num = TNum . read <$> many1 (satisfy isDigit)
t_asgn = TAsgn <$ string "->"
t_newline = TNewLine <$ lit '\n'
t_alnum = fmap mkToken $ (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)
  where mkToken "axiom" = TAxiom
        mkToken "angle" = TAngle
        mkToken i = TId 'O' --darf nie erreicht werden (siehe hack unten)

isAlphaAndUpper :: Char -> Bool
isAlphaAndUpper c = isAlpha c && isUpper c

t_id :: Parser Char Token
t_id = TId <$> satisfy isAlphaAndUpper -- übler hack!!! aber gerade keinen Plan wie das sonst gehen soll :D

-- ^ Utilities
string xs = foldr (liftA2 (:)) (pure []) $  map lit xs 
many1 p = (:) <$> p <*> many p
