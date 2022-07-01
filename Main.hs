{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use const" #-}
module Main where

import           Data.ByteString.Lazy.Char8 as BC
import           Network.HTTP.Simple        (getResponseBody, httpLBS)
import           System.IO



check :: Char -> String -> String -> Int -> IO (String, Int)
check c secret guess lives =
    case c `Prelude.elem` secret of
        True  -> do
            Prelude.putStrLn $ "\ncharacter " ++ show c ++ " present\n"
            return (combineGuess guess (Prelude.map (\char -> if c == char then char else '_') secret), lives)
        False -> do
            Prelude.putStrLn $ "\ncharacter " ++ show c ++ " not present\n"
            return (guess, lives - 1)



combineGuess ::  String -> String -> String
combineGuess old new = Prelude.zipWith (\c1 c2 -> if c1 /= '_' then c1 else if c2 /= '_' then c2 else '_') old new


isFinished :: String -> Bool
isFinished word = '_' `Prelude.notElem` word

checkStatus :: String -> String -> Int -> (String, Bool)
checkStatus sec gue l
        | isFinished gue = ("\nCongratulation you WON!", True)
        | l == 0       = ("\nsorry GAME OVER! - correct word: " ++ sec, True)
        | otherwise    = ("\ncontinue guessing", False)


initializeGuess :: String -> IO String
initializeGuess str = return $ Prelude.map (\s -> '_') str

playGame :: String -> String -> Int -> IO ()
playGame s g l = do
    Prelude.putStrLn "\nenter a character:"
    c <- getChar
    (guess, lives) <- check c s g l
    print guess
    case checkStatus s guess lives of
        (str, True)  -> Prelude.putStrLn str
        (str, False) -> do
            Prelude.putStrLn $ str ++ " - number of remaining lives: " ++ show lives
            playGame s guess lives


main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    response <- httpLBS "https://random-word-api.herokuapp.com/word?lang=en"
    let
        answer = BC.unpack $ getResponseBody response
        fAnswer = Prelude.filter (\c -> (c /= '[') && (c /= ']') && c/= '"') answer
    Prelude.putStrLn fAnswer
    Prelude.putStrLn "you can start guessing:\n"
    guess <- initializeGuess fAnswer
    Prelude.putStrLn guess
    playGame fAnswer guess 5








