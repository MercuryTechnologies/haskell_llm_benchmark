module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import Control.Concurrent.MVar

data BankAccountState = BankAccountState
    { closed  :: Bool
    , balance :: Integer
    }

type BankAccount = MVar BankAccountState

openAccount :: IO BankAccount
openAccount = newMVar (BankAccountState False 0)

closeAccount :: BankAccount -> IO ()
closeAccount account =
    modifyMVar_ account $ \s ->
        return s { closed = True }

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance account = do
    s <- readMVar account
    if closed s
       then return Nothing
       else return (Just (balance s))

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance account amount =
    modifyMVar account $ \s ->
        if closed s
           then return (s, Nothing)
           else
               let newBalance = balance s + amount
                   newState   = s { balance = newBalance }
               in return (newState, Just newBalance)
