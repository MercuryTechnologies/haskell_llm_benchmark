module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import Control.Concurrent.STM (TVar, atomically, newTVar, readTVar, writeTVar)

-- | BankAccount is represented as a TVar containing a Maybe Integer
-- Nothing represents a closed account, Just balance represents an open account with the given balance
data BankAccount = BankAccount (TVar (Maybe Integer))

-- | Opens a new bank account with an initial balance of 0
openAccount :: IO BankAccount
openAccount = do
    accountTVar <- atomically $ newTVar (Just 0)
    return $ BankAccount accountTVar

-- | Closes an account. Operations on closed accounts will fail.
closeAccount :: BankAccount -> IO ()
closeAccount (BankAccount accountTVar) = atomically $ writeTVar accountTVar Nothing

-- | Gets the balance of an account. Returns Nothing if the account is closed.
getBalance :: BankAccount -> IO (Maybe Integer)
getBalance (BankAccount accountTVar) = atomically $ readTVar accountTVar

-- | Increments (or decrements if amount is negative) the balance of an account.
-- Returns Nothing if the account is closed.
incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance (BankAccount accountTVar) amount = atomically $ do
    accountState <- readTVar accountTVar
    case accountState of
        Nothing -> return Nothing  -- Account is closed
        Just balance -> do
            let newBalance = balance + amount
            writeTVar accountTVar (Just newBalance)
            return (Just newBalance)
