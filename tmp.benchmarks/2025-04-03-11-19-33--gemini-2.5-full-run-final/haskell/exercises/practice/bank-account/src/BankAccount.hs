module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import Control.Concurrent.STM

-- BankAccount holds a TVar that contains Maybe Integer.
-- Nothing means the account is closed.
-- Just balance means the account is open with the given balance.
newtype BankAccount = BankAccount (TVar (Maybe Integer))

-- Opens a new bank account with a zero balance.
openAccount :: IO BankAccount
openAccount = do
    balanceVar <- atomically $ newTVar (Just 0)
    return (BankAccount balanceVar)

-- Closes the bank account. Subsequent operations will fail.
closeAccount :: BankAccount -> IO ()
closeAccount (BankAccount balanceVar) =
    atomically $ writeTVar balanceVar Nothing

-- Gets the balance of the account. Returns Nothing if the account is closed.
getBalance :: BankAccount -> IO (Maybe Integer)
getBalance (BankAccount balanceVar) =
    atomically $ readTVar balanceVar

-- Increments the balance of the account by the given amount (can be negative).
-- Returns the new balance if successful, or Nothing if the account is closed.
incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance (BankAccount balanceVar) amount =
    atomically $ do
        mBalance <- readTVar balanceVar
        case mBalance of
            Nothing -> return Nothing -- Account is closed
            Just currentBalance -> do
                let newBalance = currentBalance + amount
                writeTVar balanceVar (Just newBalance)
                return (Just newBalance)
