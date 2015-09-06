module GameState (
Card,
Deck,
Hand,
Spellbook,
GameState,
Action,
printCard,
drawCard,
start,
applyMove,
sendGameState
) where
import Network.Socket
import Data.List.Split

data Card = Ritual Int String Int Int String | Spell Int String Int String deriving (Show, Read)

type Deck = [Card]
type Hand = [Card]
type Spellbook = [Card]
type Player = (Deck, Hand, Spellbook, Int)
type GameState = ([Player], Int)
type Action = String

printCard :: Card -> String
printCard (Ritual _ name low high effect) = name ++ "\n" ++ (show low) ++ "-" ++ (show high) ++ "\n" ++ effect ++ "\n"
printCard (Spell _ name cost effect) = name ++ "\n" ++ (show cost) ++ "\n" ++ effect ++ "\n"

drawCard :: (Deck, Hand) -> (Deck, Hand)
drawCard ([], hand) = ([], hand)
drawCard (c:cs, hand) = (cs, c:hand)

drawNCards :: Int -> (Deck, Hand) -> (Deck, Hand)
drawNCards n = foldr (.) id (replicate n drawCard)

start :: Deck -> GameState
start p1Deck = do
    let (p1deck, p1hand) = drawNCards 7 (p1Deck, [])
        player1 = (p1deck, p1hand, [], 20)
    ([player1], 1)

--prepare :: Int -> GameState -> GameState
--prepare _ s1 = s1

--parseMove :: String -> GameState -> GameState
--parseMove m = do
--    let tokens = splitOn " " m
--    case tokens of
--        ["Prepare", _] -> prepare 0
--        _              -> prepare 0

applyMove :: String -> GameState -> GameState
--applyMove line s1 = (parseMove line) s1
applyMove _ s1 = s1

sendGameState :: Socket -> GameState -> IO ()
sendGameState sock (p1:ps, t) = do
    let (_, h1, _, _) = p1
    send sock ("Turn " ++ (show t) ++ "\n" ++ (unlines (map printCard h1)))
    send sock "Recieved."
    return ()
