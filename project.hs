import Control.Applicative
import Control.Monad
import Data.Function
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as M
import ParserCon

exampleMap :: M.Map Char [Atom]
exampleMap = M.insert 'X' [Symb DrawF, Symb PlusR, Symb DrawF, Symb MinusR, Ide 'X'  ] M.empty

-- Axiom, Angle, Map mit Befehlen -> Generation -> IO ()
executeDrawing :: Char -> Integer -> Memory -> Integer -> IO ()
executeDrawing ax ang mem gen = do 
                                putStrLn "Starting"
                                abc <- evalAtoms (M.findWithDefault [] ax mem) mem gen 0
                                putStrLn "Done"


exampleString = "angle 90\n axiom X\n X -> X+YF+\n Y -> -FX-Y"

type Log = [String]
type Memory = M.Map Char [Atom]
type Logging = WriterT Log (State Memory)
--type Logging s = Writer Log s

getMap = snd (runIt exampleString)
getNicePrint = mapM_ print $ snd $ fst $ runIt exampleString

runIt s = runState (runWriterT (parseAndEvaluate s)) M.empty

--runState (runWriterT (parseAndEvaluate s))

parseAndEvaluate :: String -> Logging () -- (Char,Integer)???
parseAndEvaluate s = case parseString s of
                      Just p -> eval p
                      Nothing -> do
                                tell ["ERROR"]

eval :: System -> Logging () 
eval (System h r) = do
                    tell ["Evaluating Headers"]
                    headers <- evalHeaders h
                    rules <- evalRules r
                    return ()

evalHeaders :: [Header] -> Logging ()
evalHeaders [] = return ()
evalHeaders (x:xs) = do
                    evalFirst <- evalHeader x 
                    evalRest <- evalHeaders xs
                    return ()


evalHeader :: Header -> Logging ()
evalHeader h = case h of
                Axiom ax -> do
                            tell ["Found Axiom " ++ show ax]
                Angle int -> do
                            tell ["Found Angle " ++ show int] --TODO STORE


evalRules :: [Rule] -> Logging ()
evalRules [] = return ()
evalRules (x:xs) = do
                    evalFirst <- evalRule x
                    evalRest <- evalRules xs
                    return ()

evalRule :: Rule -> Logging ()
evalRule (Rule i atoms) = do -- store string in state
            tell["Storing my atoms to given Id:" ++ show i]
            oldMap <- get
            let newMap = M.insert i atoms oldMap
            put newMap
            --a <- evalAtoms atoms -- wir hier nicht benötigt - execution ist unabhängig
            tell ["Done storing atomes."]


--TODO eigenen Datentyp für (Axiom, Header, Map)
evalAtoms :: [Atom] -> M.Map Char [Atom] -> Integer -> Integer -> IO ()
evalAtoms [] _ _ _ = return ()
evalAtoms (x:xs) m gen counter =  if counter >= gen then return () else 
                    do
                    putStrLn $ show counter
                    evalFirst <- evalAtom x m gen (counter)
                    evalRest <- evalAtoms xs m gen (counter)
                    return ()


evalAtom :: Atom -> M.Map Char [Atom] -> Integer -> Integer -> IO ()
evalAtom a m gen counter = case a of
                Symb s -> do
                            putStrLn $ debugString s
                Ide c -> do
                            evalAtoms (M.findWithDefault [] c m) m gen (counter+1)





debugString :: Symbol -> String 
debugString s = case s of
                DrawF -> "Draw forward"
                DrawB -> "Draw backward"
                MoveF -> "Move forward"
                MoveB -> "Move backward"
                PlusR -> "Rotate right"
                MinusR -> "Rotate left"
                Push -> "Push pos"
                Pop -> "Pop pos"


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
