module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import Control.Concurrent.STM
import Control.Monad

data BankAccount = BankAccount (TVar Integer) (TVar Bool) -- (balance, isClosed)

openAccount :: IO BankAccount
openAccount = do
    balance <- newTVarIO 0
    isClosed <- newTVarIO False
    return $ BankAccount balance isClosed

closeAccount :: BankAccount -> IO ()
closeAccount (BankAccount balance isClosed) = atomically $ writeTVar isClosed True

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance (BankAccount balance isClosed) = atomically $ do
    closed <- readTVar isClosed
    if closed
        then return Nothing
        else Just <$> readTVar balance

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance (BankAccount balance isClosed) amount = atomically $ do
    closed <- readTVar isClosed
    if closed
        then return Nothing
        else do
            currentBalance <- readTVar balance
            let newBalance = currentBalance + amount
            writeTVar balance newBalance
            return $ Just newBalance
