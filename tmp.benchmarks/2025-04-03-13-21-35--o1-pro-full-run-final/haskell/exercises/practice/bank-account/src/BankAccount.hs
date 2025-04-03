module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import Control.Concurrent.MVar

-- | A BankAccount holds an MVar that may be Nothing if closed,
--   or Just balance if open.
data BankAccount = BankAccount (MVar (Maybe Integer))

-- | Open a new BankAccount with an initial balance of 0.
openAccount :: IO BankAccount
openAccount = do
    mvar <- newMVar (Just 0)
    return (BankAccount mvar)

-- | Close the BankAccount by setting its balance to Nothing.
closeAccount :: BankAccount -> IO ()
closeAccount (BankAccount mvar) = do
    modifyMVar_ mvar $ \_ -> return Nothing

-- | Get the balance of the BankAccount, or Nothing if closed.
getBalance :: BankAccount -> IO (Maybe Integer)
getBalance (BankAccount mvar) = readMVar mvar

-- | Increment the balance by the given amount.
--   If the account is closed, return Nothing.
--   Otherwise update and return the new balance.
incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance (BankAccount mvar) amount = do
    modifyMVar mvar $ \maybeBal ->
        case maybeBal of
            Nothing -> return (Nothing, Nothing)
            Just bal ->
                let newBal = bal + amount
                in return (Just newBal, Just newBal)
