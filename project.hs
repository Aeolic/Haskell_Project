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
import DrawerM
import Options.Applicative
import Data.Semigroup ((<>))



es = "angle 90\n axiom X\n X -> X+YF+\n Y -> -FX-Y" --Dragon
es2 = "angle 90\n axiom X\n X -> XY\n Y -> -F-X-YFZ-F\n Z -> FF-FF+F"
esD = "angle 70 axiom X X -> X+YF+ Y -> -FX-Y"

es3 = "angle 120 axiom X X -> F-[+FX]-XY Y-> [X+F]X+YF+"

pad s i = parseAndDraw s i


main :: IO ()
main = pad esD  =<< execParser (info (sample <**> helper)
                      ( fullDesc
                     <> progDesc "Print a greeting for TARGET"
                     <> header "hello - a test for optparse-applicative" ))

data Sample = Sample {generations :: Integer}

sample :: Parser Sample
sample = Sample <$> option auto
                    (long "generations"
                    <> short 'g'
                    <> help "number of generations"
                    <> showDefault
                    <> value 10
                    <> metavar "INTEGER")

generationH :: Integer
generationH = 15

exampleMap :: M.Map Char [Atom]
exampleMap = M.insert 'X' [Symb DrawF, Symb PlusR, Symb DrawF, Symb MinusR, Ide 'X'] M.empty

dragonMap :: M.Map Char [Atom]
dragonMap = M.insert 'X' [Ide 'X', Symb PlusR, Ide 'Y', Symb DrawF, Symb PlusR] (M.insert 'Y' [Symb MinusR, Symb DrawF,  Ide 'X', Symb MinusR, Ide 'Y'] M.empty)

exampleState = executeDrawing  (AdvMem 'X' 90 exampleMap) generationH

dragonState = executeDrawing (AdvMem 'X' 90 dragonMap) generationH

execDragon = drawPicture $ pic $ execState dragonState myDrawer


parseAndDraw :: String -> Sample -> IO ()
parseAndDraw s (Sample gen) = drawPicture $ pic $ execState (executeDrawing (getMap s) gen) myDrawer

executeDrawing :: AdvMem -> Integer -> State Drawer () 
executeDrawing mem gen = do
                        pictures <- execAtoms (M.findWithDefault [] (axiom mem) (memory mem)) mem gen
                        return ()


--TODO eigenen Datentyp für (Axiom, Header, Map)
execAtoms :: [Atom] -> AdvMem -> Integer -> State Drawer ()
execAtoms [] _ _ = return ()
execAtoms (x:xs) m gen =  if gen <= 0 then return () else
                    do
                    evalFirst <- execAtom x m gen
                    evalRest <- execAtoms xs m gen
                    return ()


execAtom :: Atom -> AdvMem -> Integer -> State Drawer ()
execAtom a m gen = case a of
                Symb s -> do
                            symbolToDrawer s (angle m)
                Ide c -> do
                            execAtoms ( M.findWithDefault [] c (memory m)) m (gen-1)


symbolToDrawer :: Symbol -> Integer -> State Drawer ()
symbolToDrawer s i = case s of
                DrawF -> drawForward
                DrawB -> drawBackward
                MoveF -> moveForward
                MoveB -> moveBackward
                PlusR -> rotateRight (fromIntegral i)
                MinusR -> rotateLeft (fromIntegral i) 
                Push -> pushPosition
                Pop -> popPosition


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







type Log = [String]
type Memory = M.Map Char [Atom]
type Logging = WriterT Log (State AdvMem) () 

data AdvMem = AdvMem {axiom :: Char, angle :: Integer, memory :: Memory}
            deriving (Show,Eq)

getMap s = snd (runIt s)
getNicePrint = mapM_ print $ snd $ fst $ runIt es

runIt s = runState (runWriterT (parseAndEvaluate s)) (AdvMem '-' 0 M.empty)

--runState (runWriterT (parseAndEvaluate s))

parseAndEvaluate :: String -> Logging
parseAndEvaluate s = case parseString s of
                      Just p -> eval p
                      Nothing -> do
                                tell ["ERROR"]

eval :: System -> Logging
eval (System h r) = do
                    tell ["Evaluating Headers"]
                    headers <- evalHeaders h
                    rules <- evalRules r
                    return ()

evalHeaders :: [Header] -> Logging
evalHeaders [] = return ()
evalHeaders (x:xs) = do
                    evalFirst <- evalHeader x
                    evalRest <- evalHeaders xs
                    return ()


evalHeader :: Header -> Logging
evalHeader h = case h of
                Axiom axiom -> do
                            AdvMem ax ang mem <- get
                            tell ["Found Axiom " ++ show axiom]
                            put (AdvMem axiom ang mem)

                Angle int -> do
                            AdvMem ax ang mem <- get
                            tell ["Found Angle " ++ show int]
                            put (AdvMem ax int mem)


evalRules :: [Rule] -> Logging
evalRules [] = return ()
evalRules (x:xs) = do
                    evalFirst <- evalRule x
                    evalRest <- evalRules xs
                    return ()

evalRule :: Rule -> Logging
evalRule (Rule i atoms) = do -- store string in state
            tell["Storing my atoms to given Id:" ++ show i]
            AdvMem ax ang mem <- get
            let newMap = M.insert i atoms mem
            put (AdvMem ax ang newMap)
            --a <- evalAtoms atoms -- wir hier nicht benötigt - execution ist unabhängig
            tell ["Done storing atomes."]



-- ^ Parsing

type Id = Char
type NumI = Integer

data System = System [Header] [Rule]
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

parser :: ParserC Token System
parser = System <$> many (parseHeader) <*> many1 (parseRule)


parseAtom :: ParserC Token Atom
parseAtom = try (\token -> case token of
                              TSymb smb -> Just $ Symb smb
                              TId id -> Just $ Ide id
                              _ -> Nothing) --could split into 2, might be necessary for rule parser

parseId :: ParserC Token Id
parseId = try (\token -> case token of
                              TId id -> Just id
                              _ -> Nothing)

parseRule :: ParserC Token Rule --(Id parser?, many1 or many?)
parseRule = Rule <$> (parseId <* lit TAsgn) <*> (many1 parseAtom)

parseHeader :: ParserC Token Header
parseHeader = Axiom <$> (lit TAxiom *> parseId)
              <|> Angle <$> (lit TAngle *> parseNum)

parseNum :: ParserC Token NumI
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

t_id :: ParserC Char Token
t_id = TId <$> satisfy isAlphaAndUpper -- übler hack!!! aber gerade keinen Plan wie das sonst gehen soll :D

-- ^ Utilities
string xs = foldr (liftA2 (:)) (pure []) $  map lit xs
many1 p = (:) <$> p <*> many p
