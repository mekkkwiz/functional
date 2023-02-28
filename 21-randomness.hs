-- write function nRandomRs that generates a list of n random values, each in a given range
import System.Random
import Control.Monad (when)
import Text.Read (readMaybe)

nRandomRs :: (RandomGen g, UniformRange a, Integral n) => (a, a) -> n -> g -> ([a], g)
nRandomRs _ 0 gen = ([], gen)
nRandomRs range n gen =
  let (gen1, gen2) = split gen
      (x, gen3) = uniformR range gen1
      (xs, finalGen) = nRandomRs range (n - 1) gen2
  in (x : xs, finalGen)

-- Data type to represent the result of a guess (pure)
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
guessLoop :: Int -> Int -> Int -> [Int] -> IO ()
guessLoop secretNumber maxGuesses numGuesses previousGuesses = do
  when (numGuesses > maxGuesses) $ do
    putStrLn $ "Sorry, you've exceeded the maximum number of guesses (" ++ show maxGuesses ++ ")."
    putStrLn $ "The secret number was " ++ show secretNumber ++ "."
  when (numGuesses <= maxGuesses) $ do
    putStr $ "Player 2, please guess the number (guess " ++ show numGuesses ++ " of " ++ show maxGuesses ++ "): "
    input <- getLine
    case readMaybe input of
      Just guess ->
        let result = checkGuess secretNumber guess previousGuesses
            newPreviousGuesses = guess : previousGuesses
        in do
          handleGuessResult result
          case result of
            GuessCorrect -> return ()
            GuessImpossible -> guessLoop secretNumber maxGuesses numGuesses previousGuesses
            _ -> guessLoop secretNumber maxGuesses (numGuesses + 1) newPreviousGuesses
      Nothing -> do
        putStrLn "Invalid input. Please enter an integer."
        guessLoop secretNumber maxGuesses numGuesses previousGuesses

-- Entry point for the game (impure)
main :: IO ()
main = do
  putStrLn "Welcome to the number guessing game!"
  (secretNumber, maxGuesses) <- readGameParams
  guessLoop secretNumber maxGuesses 1 []
