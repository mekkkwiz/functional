-- improve your number-guessing game to report all the numbers guessed after the game is over (win or lose)

import System.Random
import Text.Read
import Data.List

nRandomRs :: (RandomGen g, UniformRange a, Integral n) => (a, a) -> n -> g -> ([a], g)
nRandomRs _ 0 gen = ([], gen)
nRandomRs range n gen =
  let (gen1, gen2) = split gen
      (x, _) = uniformR range gen1
      (xs, finalGen) = nRandomRs range (n - 1) gen2
  in (x : xs, finalGen)

data GuessResult = GuessTooLow | GuessCorrect | GuessTooHigh | GuessImpossible deriving (Eq,Show)

-- Pure function to check the guess and return a result
checkGuess :: Int -> Int -> [Int] -> GuessResult
checkGuess secretNumber guess prevGuesses
  | guess `elem` prevGuesses = GuessImpossible
  | not (null higherGuesses) && guess >= maximum higherGuesses = GuessImpossible
  | not (null lowerGuesses) && guess <= minimum lowerGuesses = GuessImpossible
  | guess < secretNumber = GuessTooLow
  | guess > secretNumber = GuessTooHigh
  | otherwise = GuessCorrect
  where higherGuesses = filter (> secretNumber) prevGuesses
        lowerGuesses = filter (< secretNumber) prevGuesses

-- Impure function to handle the result of a guess (i.e., print a message)
handleGuessResult :: GuessResult -> IO ()
handleGuessResult GuessTooLow = putStrLn "The number is higher. Try again."
handleGuessResult GuessTooHigh = putStrLn "The number is lower. Try again."
handleGuessResult GuessCorrect = putStrLn "Congratulations, you guessed the number!"
handleGuessResult GuessImpossible = putStrLn "That guess is impossible. Try again."

-- Impure function to read the parameters for the game (i.e., secret number and maximum number of guesses)
readGameParams :: IO (Int, Int)
readGameParams = do
  maxNumber <- readParam "Please enter the maximum number for the game: "
  maxGuesses <- readParam "Please enter the maximum number of guesses for the game: "
  secretNumber <- randomizeSecretNumber maxNumber
  return (secretNumber, maxGuesses)

-- Impure function to prompt the user to enter a parameter and read the input (with error handling)
readParam :: String -> IO Int
readParam prompt = do
  putStr prompt
  input <- getLine
  case readMaybe input of
    Just param -> return param
    Nothing -> do
      putStrLn "Invalid input. Please enter an integer."
      readParam prompt

-- Pure function to generate a random secret number for the game
randomizeSecretNumber :: Int -> IO Int
randomizeSecretNumber maxNumber = do
  gen <- newStdGen
  let (randNum:_) = fst $ nRandomRs (1, maxNumber) 1 gen
  return randNum

-- Impure function to run the main game loop
-- Impure function to run the main game loop and return the list of guesses
guessLoop :: Int -> Int -> Int -> [Int] -> IO [Int]
guessLoop secretNumber maxGuesses numGuesses previousGuesses
  | numGuesses > maxGuesses = do
      putStrLn $ "Sorry, you've exceeded the maximum number of guesses (" ++ show maxGuesses ++ ")."
      putStrLn $ "The secret number was " ++ show secretNumber ++ "."
      putStrLn "You lose!"
      return previousGuesses
  | otherwise = do
      putStr $ "Player 2, please guess the number (guess " ++ show numGuesses ++ " of " ++ show maxGuesses ++ "): "
      input <- getLine
      case readMaybe input of
        Just guess -> do
          let result = checkGuess secretNumber guess previousGuesses
              newPreviousGuesses = guess : previousGuesses
          handleGuessResult result
          case result of
            GuessCorrect -> do
              putStrLn "You win!"
              return newPreviousGuesses
            GuessImpossible -> guessLoop secretNumber maxGuesses numGuesses previousGuesses
            _ -> guessLoop secretNumber maxGuesses (numGuesses + 1) newPreviousGuesses
        Nothing -> do
          putStrLn "Invalid input. Please enter an integer."
          guessLoop secretNumber maxGuesses numGuesses previousGuesses

main :: IO ()
main = do
  putStrLn "Welcome to the number guessing game!"
  (secretNumber, maxGuesses) <- readGameParams
  guesses <- guessLoop secretNumber maxGuesses 1 []
  putStrLn $ "Guesses made: " ++ intercalate ", " (map show (reverse guesses))

