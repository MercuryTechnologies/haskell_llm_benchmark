module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import Control.Concurrent.STM
import Control.Monad (when)

-- | A BankAccount is a transactional variable holding Maybe Integer.
--   'Nothing' means the account is closed.
newtype BankAccount = BankAccount (TVar (Maybe Integer))

-- | Open a new account with initial balance 0.
openAccount :: IO BankAccount
openAccount = do
    tvar <- newTVarIO (Just 0)
    return (BankAccount tvar)

-- | Close the account. After this, all operations will fail.
closeAccount :: BankAccount -> IO ()
closeAccount (BankAccount tvar) = atomically $ writeTVar tvar Nothing

-- | Get the current balance, or Nothing if the account is closed.
getBalance :: BankAccount -> IO (Maybe Integer)
getBalance (BankAccount tvar) = atomically $ readTVar tvar

-- | Increment the balance by the given amount (can be negative).
--   Returns the new balance, or Nothing if the account is closed.
incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance (BankAccount tvar) amount = atomically $ do
    mbal <- readTVar tvar
    case mbal of
        Nothing -> return Nothing
        Just bal -> do
            let newBal = bal + amount
            writeTVar tvar (Just newBal)
            return (Just newBal)
