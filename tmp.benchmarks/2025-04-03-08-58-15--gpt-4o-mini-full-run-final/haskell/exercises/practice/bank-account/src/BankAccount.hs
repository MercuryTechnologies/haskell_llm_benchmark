module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import Control.Concurrent.STM
import Control.Monad (when)

data BankAccount = BankAccount
    { balance :: TVar Integer
    , isClosed :: TVar Bool
    }

closeAccount :: BankAccount -> IO ()
closeAccount account = atomically $ writeTVar (isClosed account) True

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance account = atomically $ do
    closed <- readTVar (isClosed account)
    if closed
        then return Nothing
        else readTVar (balance account)

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance account amount = atomically $ do
    closed <- readTVar (isClosed account)
    when closed $ error "Account is closed."
    currentBalance <- readTVar (balance account)
    let newBalance = currentBalance + amount
    writeTVar (balance account) newBalance
    return (Just newBalance)

openAccount :: IO BankAccount
openAccount = atomically $ do
    balanceVar <- newTVar 0
    closedVar <- newTVar False
    return $ BankAccount balanceVar closedVar
