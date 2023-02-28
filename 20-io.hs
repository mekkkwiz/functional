-- write a number-guessing game
-- the first player enters the desired number
-- we'll deal with generating random numbers automatically next lecture
-- then, the second player keeps guessing the number until correct

-- import Control.Monad (when)
-- import Text.Read (readMaybe)

-- main :: IO ()
-- main = do
--   putStrLn "Welcome to the number guessing game!"
--   -- First player enters the desired number
--   secretNumber <- readSecretNumber
--   -- Second player tries to guess the number
--   guessLoop secretNumber

-- readSecretNumber :: IO Int
-- readSecretNumber = do
--   putStr "Player 1, please enter the secret number: "
--   input <- getLine
--   case readMaybe input of
--     Just secretNumber -> return secretNumber
--     Nothing -> do
--       putStrLn "Invalid input. Please enter an integer."
--       readSecretNumber

-- guessLoop :: Int -> IO ()
-- guessLoop secretNumber = do
--   putStr "Player 2, please guess the number: "
--   input <- getLine
--   case readMaybe input of
--     Just guess -> do
--       if guess == secretNumber
--         then putStrLn "Congratulations, you guessed the number!"
--         else do
--           when (guess < secretNumber) $ putStrLn "The number is higher. Try again."
--           when (guess > secretNumber) $ putStrLn "The number is lower. Try again."
--           guessLoop secretNumber
--     Nothing -> do
--       putStrLn "Invalid input. Please enter an integer."
--       guessLoop secretNumber


-- try to separate your code into pure and impure parts
--      the impure parts interact with the users
--      the pure parts deal with game logic
--      each part should be a function

-- import Control.Monad (when)
-- import Text.Read (readMaybe)

-- data GuessResult = GuessTooLow | GuessCorrect | GuessTooHigh deriving (Eq,Show)

-- -- impure functions

-- main :: IO ()
-- main = do
--   putStrLn "Welcome to the number guessing game!"
--   secretNumber <- readSecretNumber
--   guessLoop secretNumber


-- readSecretNumber :: IO Int
-- readSecretNumber = do
--   putStr "Player 1, please enter the secret number: "
--   input <- getLine
--   case readMaybe input of
--     Just secretNumber -> return secretNumber
--     Nothing -> do
--       putStrLn "Invalid input. Please enter an integer."
--       readSecretNumber

-- guessLoop :: Int -> IO ()
-- guessLoop secretNumber = do
--   putStr "Player 2, please guess the number: "
--   input <- getLine
--   case readMaybe input of
--     Just guess -> do
--       let result = checkGuess secretNumber guess
--       handleGuessResult result
--       when (result /= GuessCorrect) $ guessLoop secretNumber
--     Nothing -> do
--       putStrLn "Invalid input. Please enter an integer."
--       guessLoop secretNumber

-- -- pure functions

-- checkGuess :: Int -> Int -> GuessResult
-- checkGuess secretNumber guess
--   | guess < secretNumber = GuessTooLow
--   | guess > secretNumber = GuessTooHigh
--   | otherwise = GuessCorrect

-- handleGuessResult :: GuessResult -> IO ()
-- handleGuessResult GuessTooLow = putStrLn "The number is higher. Try again."
-- handleGuessResult GuessTooHigh = putStrLn "The number is lower. Try again."
-- handleGuessResult GuessCorrect = putStrLn "Congratulations, you guessed the number!"


-- improve your number-guessing game to limit the number of guesses
-- this number should also be entered by the first player

import Control.Monad (when)
import Text.Read (readMaybe)

-- Data type to represent the result of a guess (pure)
data GuessResult = GuessTooLow | GuessCorrect | GuessTooHigh deriving (Eq,Show)

-- Pure function to check the guess and return a result
checkGuess :: Int -> Int -> GuessResult
checkGuess secretNumber guess
  | guess < secretNumber = GuessTooLow
  | guess > secretNumber = GuessTooHigh
  | otherwise = GuessCorrect

-- Impure function to handle the result of a guess (i.e., print a message)
handleGuessResult :: GuessResult -> IO ()
handleGuessResult GuessTooLow = putStrLn "The number is higher. Try again."
handleGuessResult GuessTooHigh = putStrLn "The number is lower. Try again."
handleGuessResult GuessCorrect = putStrLn "Congratulations, you guessed the number!"

-- Impure function to read the parameters for the game (i.e., secret number and maximum number of guesses)
readGameParams :: IO (Int, Int)
readGameParams = do
  secretNumber <- readParam "Player 1, please enter the secret number: "
  maxGuesses <- readParam "Player 1, please enter the maximum number of guesses: "
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

-- Impure function to run the main game loop
guessLoop :: Int -> Int -> Int -> IO ()
guessLoop secretNumber maxGuesses numGuesses = do
  when (numGuesses > maxGuesses) $ do
    putStrLn $ "Sorry, you've exceeded the maximum number of guesses (" ++ show maxGuesses ++ ")."
    putStrLn $ "The secret number was " ++ show secretNumber ++ "."
  when (numGuesses <= maxGuesses) $ do
    putStr $ "Player 2, please guess the number (guess " ++ show numGuesses ++ " of " ++ show maxGuesses ++ "): "
    input <- getLine
    case readMaybe input of
      Just guess -> do
        let result = checkGuess secretNumber guess
        handleGuessResult result
        when (result /= GuessCorrect) $ guessLoop secretNumber maxGuesses (numGuesses + 1)
      Nothing -> do
        putStrLn "Invalid input. Please enter an integer."
        guessLoop secretNumber maxGuesses numGuesses

-- Entry point for the game (impure)
main :: IO ()
main = do
  putStrLn "Welcome to the number guessing game!"
  (secretNumber, maxGuesses) <- readGameParams
  guessLoop secretNumber maxGuesses 1


-- can you reuse code from the previous version?

-- Yes, some of the code has been reused from the previous version.
-- Specifically, the checkGuess function and the handleGuessResult function are the same as in the previous version.
-- However, the readParam function has been updated to handle non-integer input, and the readSecretNumber
-- and readMaxGuesses functions have been combined into a single readGameParams function.
-- The guessLoop function has been updated to include a check for the maximum number of guesses
-- and to print an correct message when the maximum number of guesses has been reached.
-- The main function has also been updated to use the readGameParams function instead
-- of the readSecretNumber and readMaxGuesses functions.