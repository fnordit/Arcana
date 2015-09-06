import Reactive.Banana
import Reactive.Banana.Frameworks
import Data.Char
import Control.Monad
import Control.Concurrent
import Control.Event.Handler
import Control.Concurrent.MVar
import Data.Bool
import System.Exit
import Network.Socket
import Server
import GameState

createServerHandler :: Handler String -> MVar Bool -> HandlerFunc
createServerHandler pushEvent sem sock msg = do
    pushEvent msg
    if msg == "exit"
        then putMVar sem True
        else return ()

makeGameChannel :: Frameworks t => AddHandler String -> Socket -> GameState -> Moment t ()
makeGameChannel addHandler p1sock startState = do
    eMove <- fromAddHandler addHandler
    let bGameState = accumB startState (fmap applyMove eMove)
    eGameStateChange <- changes bGameState
    reactimate' $ fmap (sendGameState p1sock) <$> eGameStateChange -- Why does this work?


main :: IO()
main = do
    -- reading decks
    p1DeckRaw <- readFile "test.dec"
    let p1DeckList = lines p1DeckRaw
        p1Deck = map read p1DeckList
    -- initialize event stream
    (addHandler, pushEvent) <- newAddHandler
    sem <- newEmptyMVar
    let netHand = createServerHandler pushEvent sem
    p1sock <- server "4242" netHand
    gameLog <- compile (makeGameChannel addHandler p1sock (start p1Deck))
    actuate gameLog
    quit <- takeMVar sem
    exitSuccess
