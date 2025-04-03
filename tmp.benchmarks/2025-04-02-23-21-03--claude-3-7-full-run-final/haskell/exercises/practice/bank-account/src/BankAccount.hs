module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import Control.Concurrent.STM
import Control.Monad (when)

-- BankAccount consists of:
-- 1. A TVar to track if the account is open (True) or closed (False)
-- 2. A TVar to store the current balance
data BankAccount = BankAccount
    { accountStatus :: TVar Bool
    , accountBalance :: TVar Integer
    }

-- Open a new account with zero balance
openAccount :: IO BankAccount
openAccount = atomically $ do
    status <- newTVar True  -- Account starts as open
    balance <- newTVar 0    -- Initial balance is 0
    return $ BankAccount status balance

-- Close an account
closeAccount :: BankAccount -> IO ()
closeAccount account = atomically $ writeTVar (accountStatus account) False

-- Get the current balance of an account
-- Returns Nothing if the account is closed
getBalance :: BankAccount -> IO (Maybe Integer)
getBalance account = atomically $ do
    isOpen <- readTVar (accountStatus account)
    if isOpen
        then Just <$> readTVar (accountBalance account)
        else return Nothing

-- Increment (or decrement if amount is negative) the account balance
-- Returns the new balance, or Nothing if the account is closed
incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance account amount = atomically $ do
    isOpen <- readTVar (accountStatus account)
    if not isOpen
        then return Nothing
        else do
            currentBalance <- readTVar (accountBalance account)
            let newBalance = currentBalance + amount
            writeTVar (accountBalance account) newBalance
            return $ Just newBalance
