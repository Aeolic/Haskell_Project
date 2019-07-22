import Data.Function
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad.State
import Control.Monad.Except
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
    Sample {generations :: Int, animate :: Bool, colorful :: Bool, framesPerSecond :: Int, target :: String}

sample :: Parser Sample
sample = Sample <$> option positiveNumber
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
                <*> switch
                    ( long "colorful"
                    <> short 'c'
                    <> help "Life is beautiful." )
                <*> option positiveNumber
                    (long "frames"
                    <> short 'f'
                    <> help "Number of Frames drawn per second if program is started with '--animate'."
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

positiveNumber :: ReadM Int
positiveNumber = do
  i <- auto
  case (\i -> if i > 0 then True else False) i of
   True  -> return i
   False -> readerError "Number must be >0"

-- 2. Top level execution + helper methods

main :: IO ()
main = do
        parseAndDraw =<< execParser (info (sample <**> helper)
                      ( fullDesc
                     <> progDesc "Executes a drawing for the L-System specified in 'system.txt'!"
                     <> header "Tamara and Jureks amazing haskell drawing machine." ))

parseAndDraw :: Sample -> IO ()
parseAndDraw (Sample gen False c _ target) = do
                                contents <- readFile target
                                let (res, advMem) = getAdvMem contents
                                case res of
                                  (Right _) -> do
                                             myPic <- evalStateT (executeDrawing advMem gen) (makeMyDrawer c)
                                             drawPicture $ myPic
                                  (Left s) -> putStrLn s

parseAndDraw (Sample gen True c steps target) = do
                                contents <- readFile target
                                let (res, advMem) = getAdvMem contents
                                case res of
                                  (Right _) -> do
                                              finalPic <- evalStateT (executeDrawing (advMem) gen) (makeMyDrawer c)
                                              let startPic = (Pictures[Blank],finalPic)
                                              simulate (InWindow "Animation" (1200, 800) (0, 0))
                                                  white steps startPic modToPic getNewPicture
                                  (Left s) -> putStrLn s

-- Execution without animation
executeDrawing :: AdvMem -> Int -> MyState Picture
executeDrawing mem gen = do
                        execAtoms (axiom mem) mem gen
                        pictures <- get
                        return (Pictures (reverse $ getPics $ pic $ pictures)) -- das ist schon mit Abstand die geilste Zeile code :D

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
type Memory = M.Map Atom ProbMap
type LSystem = ExceptT String (State AdvMem) ()
type Command = [Atom]

type ProbMap = M.Map Float [Atom]

data AdvMem = AdvMem {axiom :: [Atom], angle :: Int, memory :: Memory}
            deriving (Show,Eq)

--getAdvMem :: String -> AdvMem
getAdvMem s = runState (runExceptT (parseAndEvaluate s)) (AdvMem [] 0 M.empty)

parseAndEvaluate :: String -> LSystem
parseAndEvaluate s = do
  case parseString s of
    Just p -> eval p
    Nothing -> throwError "Error: Couldn't parse L-system"

eval :: System -> LSystem
eval (System h r) = do
                    headers <- evalHeaders h
                    rules <- evalRules r
                    return ()

evalHeaders :: [Header] -> LSystem
evalHeaders [] = do
                 AdvMem ax ang mem <- get
                 case ax of
                   [] -> throwError "Error: No Axiom found"
                   _ -> return ()
evalHeaders (x:xs) = do
                    evalFirst <- evalHeader x
                    evalRest <- evalHeaders xs
                    return evalRest


evalHeader :: Header -> LSystem
evalHeader h = case h of
                Axiom axiom -> do
                            AdvMem ax ang mem <- get
                            put (AdvMem axiom ang mem)

                Angle int -> do
                            AdvMem ax ang mem <- get
                            put (AdvMem ax int mem)


evalRules :: [Rule] -> LSystem
evalRules [] = do
              AdvMem ax ang mem <- get
              let rules = M.keys mem
              let maxProbs = foldr (\i acc -> (fst (M.findMax (M.findWithDefault M.empty i mem))):acc) [] rules
              let minOfMaxProbs = foldr (min) 1.0 maxProbs
              let maxOfMaxProbs = foldr (max) 1.0 maxProbs
              case (minOfMaxProbs, maxOfMaxProbs) of
                (1.0,1.0) -> return ()
                _ ->  throwError "Error: Probabilities don't add up"
              return ()
evalRules (x:xs) = do
                    evalFirst <- evalRule x
                    evalRest <- evalRules xs
                    return evalRest

evalRule :: Rule -> LSystem
evalRule (Rule i atoms) = do
            AdvMem ax ang mem <- get
            let prob1Map = M.insert 1.0 atoms M.empty
            let newMap = M.insert i prob1Map mem
            put (AdvMem ax ang newMap)
            return ()

evalRule (RuleP i prob atoms) = do
            AdvMem ax ang mem <- get
            let probMapOfI = M.findWithDefault M.empty i mem
            let probSum = (foldr (max) 0 (M.keys probMapOfI)) + prob
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
data Header = Axiom [Atom] | Angle Int
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
parseHeader = Axiom <$> (lit TAxiom *> (many1 parseAtom) )
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
