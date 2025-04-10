module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , updateBalance
    , openAccount
    ) where

import Control.Concurrent.STM

newtype BankAccount = BankAccount (TVar (Bool, Integer))

openAccount :: IO BankAccount
openAccount = atomically $ do
    var <- newTVar (True, 0)
    return (BankAccount var)

closeAccount :: BankAccount -> IO ()
closeAccount (BankAccount var) = atomically $ do
    (open, balance) <- readTVar var
    writeTVar var (False, balance)

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance (BankAccount var) = atomically $ do
    (open, balance) <- readTVar var
    if open then return (Just balance) else return Nothing

updateBalance :: BankAccount -> Integer -> IO (Maybe Integer)
updateBalance (BankAccount var) amount = atomically $ do
    (open, balance) <- readTVar var
    if open then do
        let newBalance = balance + amount
        writeTVar var (True, newBalance)
        return (Just newBalance)
    else return Nothing
