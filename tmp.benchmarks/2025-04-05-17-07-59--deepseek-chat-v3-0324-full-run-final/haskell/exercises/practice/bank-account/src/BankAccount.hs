module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import Control.Concurrent.STM

data BankAccount = BankAccount (TVar (Maybe Integer))

closeAccount :: BankAccount -> IO ()
closeAccount (BankAccount balanceVar) = atomically $ writeTVar balanceVar Nothing

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance (BankAccount balanceVar) = atomically $ readTVar balanceVar

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance (BankAccount balanceVar) amount = atomically $ do
    currentBalance <- readTVar balanceVar
    case currentBalance of
        Nothing -> return Nothing
        Just balance -> do
            let newBalance = balance + amount
            writeTVar balanceVar (Just newBalance)
            return (Just newBalance)

openAccount :: IO BankAccount
openAccount = atomically $ BankAccount <$> newTVar (Just 0)
