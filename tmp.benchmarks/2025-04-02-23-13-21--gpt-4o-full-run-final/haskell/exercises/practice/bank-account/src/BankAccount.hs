module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import Control.Concurrent.STM
import Control.Monad (when)

data BankAccount = BankAccount (TVar (Maybe Integer))

openAccount :: IO BankAccount
openAccount = do
    balance <- newTVarIO (Just 0)
    return $ BankAccount balance

closeAccount :: BankAccount -> IO ()
closeAccount (BankAccount balance) = atomically $ writeTVar balance Nothing

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance (BankAccount balance) = atomically $ readTVar balance

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance (BankAccount balance) amount = atomically $ do
    currentBalance <- readTVar balance
    case currentBalance of
        Nothing -> return Nothing
        Just b -> do
            let newBalance = b + amount
            writeTVar balance (Just newBalance)
            return (Just newBalance)
