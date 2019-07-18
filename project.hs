import Control.Applicative
import Control.Monad
import Data.Function
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map.Strict as M
import ParserCon
import DrawerM
import Options.Applicative
import Data.Semigroup ((<>))
import System.Random
import Graphics.Gloss.Data.ViewPort

{-
Sortierst du noch DrawerM ein bisschen? :D
Wir könnten noch Farben machen + Automatischen Viewport, dass man immer das ganze Bild sieht bei ner Animation
-}

-- 1. Der Parser fürs CLI
data Sample = 
    Sample {generations :: Int, animate :: Bool, framesPerSecond :: Int, target :: String}

sample :: Parser Sample
sample = Sample <$> option auto
                    (long "generations"
                    <> short 'g'
                    <> help "Number of generations."
                    <> showDefault
                    <> value 7
                    <> metavar "Int")
                <*> switch
                    ( long "animate"
                    <> short 'a'
                    <> help "Whether the drawing is animated or static." )
                <*> option auto
                    (long "frames"
                    <> short 'f'
                    <> help "Number of Frames drawn per second, when program is start with '--animate'."
                    <> showDefault
                    <> value 20
                    <> metavar "Int")
                <*> strOption
                    ( long "target"
                    <> short 't'
                    <> metavar "TARGET"
                    <> help "Target for the greeting"
                    <> showDefault
                    <> value "system.txt" )

-- 2. Top level execution + helper methods

main :: IO ()
main = do
        parseAndDraw =<< execParser (info (sample <**> helper)
                      ( fullDesc
                     <> progDesc "Executes a drawing for the L-System specified in 'system.txt'!"
                     <> header "Tamara and Jureks amazing haskell drawing machine." ))

parseAndDraw :: Sample -> IO ()
parseAndDraw (Sample gen False _ target) = do 
                                contents <- readFile target
                                myPic <- evalStateT (executeDrawing (getAdvMem contents) gen) myDrawer
                                drawPicture $ myPic
parseAndDraw (Sample gen True steps target) = do
                                contents <- readFile target
                                startPic <- getPictures contents gen
                                simulate (InWindow "Animation" (1200, 800) (0, 0))
                                    white steps startPic modToPic getNewPicture

-- Execution without animation
executeDrawing :: AdvMem -> Int -> MyState Picture
executeDrawing mem gen = do
                        execProbMap (M.findWithDefault M.empty (Ide(axiom mem)) (memory mem)) mem gen
                        pictures <- get
                        return (Pictures (reverse $ getPics $ pic $ pictures)) -- das ist schon mit Abstand die geilste Zeile code :D

-- Execution with animation
getPictures :: String -> Int -> IO (Picture,Picture)
getPictures s gen = do
                    myPic <- evalStateT (executeDrawing (getAdvMem s) gen) myDrawer
                    return (Pictures[Blank], myPic)

modToPic :: (Picture,Picture) -> Picture
modToPic (x,y) = x


getNewPicture :: ViewPort -> Float -> (Picture,Picture) -> (Picture,Picture)
getNewPicture v f ( p,Pictures (y:ys)) = (Pictures [p,y] ,Pictures ys)
getNewPicture v f (p, Pictures []) = (p, Pictures [])

-- Helper for randomized L-systems
getRandomFloat :: IO Float
getRandomFloat = do 
        num <- randomRIO (0,100)
        return (num/100)


-- 3. Hier werden die Atome ausgeführt, das heißt eine Liste aus Bilder wird erstellt
execProbMap :: ProbMap -> AdvMem -> Int ->  MyState ()
execProbMap pM m gen = do
                    randFloat <- liftIO $ getRandomFloat
                    let key = M.lookupGE randFloat pM
                    case key of 
                        Just (k,v) -> do
                                     execAtoms v m gen
                        Nothing -> do
                                    return()
                    return ()


execAtoms :: [Atom] -> AdvMem -> Int ->  MyState ()
execAtoms [] _ _ = return ()
execAtoms (x:xs) m gen =  if gen <= 0 then return () else
                    do
                    evalFirst <- execAtom x m gen
                    evalRest <- execAtoms xs m gen
                    return ()


execAtom :: Atom -> AdvMem -> Int ->  MyState ()
execAtom a m gen = case a of
                Symb s -> do
                            symbolToDrawer s (angle m)
                            execProbMap ( M.findWithDefault M.empty (Symb s) (memory m)) m (gen-1)
                Ide c -> do
                            execProbMap ( M.findWithDefault M.empty (Ide c) (memory m)) m (gen-1)


symbolToDrawer :: Symbol -> Int ->  MyState ()
symbolToDrawer s i = case s of
                DrawF -> drawForward
                DrawB -> drawBackward
                MoveF -> moveForward
                MoveB -> moveBackward
                PlusR -> rotateRight (fromIntegral i)
                MinusR -> rotateLeft (fromIntegral i) 
                Push -> pushPosition
                Pop -> popPosition


-- 4. Evaluierung von geparsten Elementen zu Memory
--TODO rename, remove unnecessary writer monad
type Log = [String]
type Memory = M.Map Atom ProbMap
type Logging = WriterT Log (State AdvMem) () 
type Command = [Atom]

type ProbMap = M.Map Float [Atom]

data AdvMem = AdvMem {axiom :: Char, angle :: Int, memory :: Memory}
            deriving (Show,Eq)

-- parses and then evaluates a given String and returns an AdvMem
getAdvMem :: String -> AdvMem
getAdvMem s = execState (runWriterT (parseAndEvaluate s)) (AdvMem '-' 0 M.empty)

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
evalRule (Rule i atoms) = do
            AdvMem ax ang mem <- get
            let prob1Map = M.insert 1.0 atoms M.empty
            let newMap = M.insert i prob1Map mem
            put (AdvMem ax ang newMap)
            return ()

evalRule (RuleP i prob atoms) = do
            AdvMem ax ang mem <- get
            let probMapOfI = M.findWithDefault M.empty i mem 
            let probSum = (foldr (+) 0 (M.keys probMapOfI)) + prob
            let probMap = M.insert probSum atoms probMapOfI
            let newMap = M.insert i probMap mem
            put (AdvMem ax ang newMap)
            return ()



-- 5. Parsing: Parst gelexten String zu Datentypen

type Id = Char
type NumI = Int
type Prob = Float

data System = System [Header] [Rule]
     deriving (Show, Eq)
data Header = Axiom Id | Angle Int
  deriving (Show, Eq)
data Rule = Rule Atom [Atom] | RuleP Atom Prob [Atom]
  deriving (Show, Eq)
data Atom = Symb Symbol | Ide Char
  deriving (Show, Eq, Ord)
data Symbol = DrawF | DrawB | MoveF| MoveB | PlusR | MinusR | Push | Pop
  deriving(Show, Eq, Ord)


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
parseRule = Rule <$> (parseAtom <* lit TAsgn) <*> (many1 parseAtom)
            <|>
            RuleP <$> parseAtom <*> parseProb <*> (many1 parseAtom)

parseHeader :: ParserC Token Header
parseHeader = Axiom <$> (lit TAxiom *> parseId)
              <|> Angle <$> (lit TAngle *> parseNum)

parseNum :: ParserC Token NumI
parseNum = try (\token -> case token of
                              TNum x-> Just x
                              _ -> Nothing)

parseProb :: ParserC Token Prob
parseProb = try (\token -> case token of
                              TProb x-> Just x
                              _ -> Nothing)


-- 6. Lexer: String -> Tokens
-- Use this lexer to tokenize the input before parsing
data Token = TAsgn -- '->'
           | TNum Int
           | TId Id
           | TSymb Symbol
           | TNewLine
           | TAxiom
           | TAngle
           | TProb Float
  deriving (Eq, Show)

lexer :: String -> Maybe [Token]
lexer = parse $ many1 (skipSpace *> p_tok) <* skipSpace

skipSpace = many (satisfy isSpace)
p_tok :: ParserC Char Token
p_tok =
  t_newline
  <|> t_num
  <|> t_asgn
  <|> t_symbol
  <|> t_id
  <|> t_alnum
  <|> t_prob


t_symbol = fmap TSymb $
         DrawF <$ lit 'F'
        <|> DrawB <$ lit 'B'
        <|> MoveF <$ lit 'f'
        <|> MoveB <$ lit 'b'
        <|> PlusR <$ lit '+'
        <|> MinusR <$ lit '-'
        <|> Push <$ lit '['
        <|> Pop <$ lit ']'

t_prob :: ParserC Char Token
t_prob = TProb . read <$> (string "-" *>  many1 ((satisfy isDigit)<|> lit '.') <* lit '>')
t_num = TNum . read <$> many1 (satisfy isDigit)
t_asgn = TAsgn <$ string "->"
t_newline = TNewLine <$ lit '\n'
t_alnum = fmap mkToken $ (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)
  where mkToken "axiom" = TAxiom
        mkToken "angle" = TAngle
        mkToken i = TId 'O' --darf nie erreicht werden 

isAlphaAndUpper :: Char -> Bool
isAlphaAndUpper c = isAlpha c && isUpper c

t_id :: ParserC Char Token
t_id = TId <$> satisfy isAlphaAndUpper

-- ^ Utilities
string xs = foldr (liftA2 (:)) (pure []) $  map lit xs
many1 p = (:) <$> p <*> many p
