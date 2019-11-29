-- Rafał Trypus
-- rt386444

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Control.Monad.State.Lazy
import Control.Monad.Except
import Lib
import Mon
import System.Environment
import Text.Read hiding (get)

firstLine = "300 400 translate"
lastLine = "stroke showpage"
errorLine = "/Courier findfont 24 scalefont setfont 0 0 moveto (Error) show"

data MyError = EmptyStack | DivisionByZero | CurrPointNotDefined | IntructionUnknown

data ProgramState = 
    ProgramState {
        stack         :: [R],
        startPoint    :: Point,
        currPoint     :: Point,
        currLength    :: Int,
        currTransform :: Transform,
        picture       :: Picture
    }

startState =
    ProgramState { 
        stack          = [],
        startPoint     = point (0, 0),
        currPoint      = point (0, 0),
        currLength     = -1,
        currTransform  = Tr [],
        picture        = Pic []
    }

newtype MyMonad a = MyMonad(StateT ProgramState (ExceptT MyError IO) a) 
    deriving (Functor, Applicative, Monad, MonadIO, MonadState ProgramState, MonadError MyError)

maybeHead :: [R] -> MyMonad(R)
maybeHead [] = throwError EmptyStack
maybeHead (h:t) = modify (\s -> s {stack = t}) >> return h

popStack :: MyMonad (R)
popStack = gets stack >>= maybeHead

popStack2 :: MyMonad (R, R)
popStack2 = (\x y -> return(y, x)) <$> popStack <*> popStack >>= id

pushToStack :: R -> MyMonad ()
pushToStack x = modify (\s -> s{stack = x : stack s})

executeAdd :: (R, R) -> MyMonad ()
executeAdd (el1, el2) = modify (\s -> s {stack = (+) el1 el2 : stack s})

executeSub :: (R, R) -> MyMonad ()
executeSub (el1, el2) = modify (\s -> s {stack = (-) el1 el2 : stack s})

executeMul :: (R, R) -> MyMonad ()
executeMul (el1, el2) = modify (\s -> s {stack = (*) el1 el2 : stack s})

executeDiv :: (R, R) -> MyMonad ()
executeDiv (_, 0) = throwError DivisionByZero
executeDiv (el1, el2) = modify (\s -> s {stack = (/) el1 el2 : stack s})

executeMoveTo :: (R, R) -> Transform -> MyMonad ()
executeMoveTo coord currTransform =
    let trPoint = trpoint currTransform (point coord) in
    modify $ \s -> s {startPoint = trPoint, currPoint = trPoint, currLength = 0}

executeLineTo :: (R, R) -> Transform -> Int -> MyMonad ()
executeLineTo coord currTransform currPathLength
    | currPathLength >= 0 =
        let trPoint = trpoint currTransform (point coord) in
        modify $ \s -> s {
            picture = picture s & pLine (currPoint s) trPoint,
            currPoint = trPoint, 
            currLength = currLength s + 1}
    | otherwise = throwError CurrPointNotDefined

executeClosePath :: Int -> MyMonad ()
executeClosePath currPathLength 
    | currPathLength > 0 =  
        modify $ \s -> s {
            picture = picture s & pLine (currPoint s) (startPoint s),
            currPoint = startPoint s,
            currLength = currLength s + 1}
    | otherwise = return()

executeTranslate :: (R, R) -> MyMonad ()
executeTranslate coord = modify $ \s -> s {currTransform = translate (vec coord) >< currTransform s}

executeRotate :: R -> MyMonad ()
executeRotate el = modify $ \s -> s {currTransform = rotate el >< currTransform s}

renderOutput :: IntRendering -> String
renderOutput [] = ""
renderOutput (((x1, y1), (x2, y2)):t) =
    (show x1 ++ " " ++ show y1 ++ " moveto " ++ show x2 ++ " " ++ show y2 ++ " lineto\n") ++
    renderOutput t

processInput :: Int -> [String] -> MyMonad()
processInput n [] = gets picture >>= liftIO . putStr . renderOutput . renderScaled n
processInput n (h:t) = do
    case readInt h of
        Just n -> pushToStack (toRational n)
        _      -> case () of
            () | h == "add" -> popStack2 >>= executeAdd
               | h == "sub" -> popStack2 >>= executeSub
               | h == "div" -> popStack2 >>= executeDiv
               | h == "mul" -> popStack2 >>= executeMul
               | h == "moveto" -> executeMoveTo <$> popStack2 <*> (gets currTransform) >>= id
               | h == "lineto" -> executeLineTo <$> popStack2 <*> (gets currTransform) <*> (gets currLength) >>= id
               | h == "closepath" -> gets currLength >>= executeClosePath
               | h == "translate" -> popStack2 >>= executeTranslate
               | h == "rotate" -> popStack >>= executeRotate
               | otherwise -> throwError IntructionUnknown
    processInput n t 

readInt :: String -> Maybe Int
readInt (h:t) = if h == '+' then readMaybe t else readMaybe (h:t)

parseArgs :: [String] -> Maybe Int
parseArgs [] = Just 1
parseArgs (h:[]) = readInt h
parseArgs _ = Nothing

errorHandler :: MyError -> MyMonad()
errorHandler _ = liftIO $ putStrLn errorLine

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of 
    Just n -> do
        putStrLn firstLine
        input <- getContents
        let MyMonad m = (processInput n (words input) `catchError` errorHandler)
        runExceptT $ evalStateT m startState
        putStrLn lastLine
    Nothing -> putStrLn "Usage: ./Main scale"