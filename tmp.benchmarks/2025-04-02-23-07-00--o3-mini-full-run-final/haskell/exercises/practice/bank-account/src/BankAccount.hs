module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import Control.Concurrent.STM

data AccountData = AccountData
    { balance :: Integer
    , open :: Bool
    }

newtype BankAccount = BankAccount (TVar AccountData)

openAccount :: IO BankAccount
openAccount = do
    tvar <- newTVarIO (AccountData 0 True)
    return (BankAccount tvar)

closeAccount :: BankAccount -> IO ()
closeAccount (BankAccount tvar) = atomically $ do
    acc <- readTVar tvar
    writeTVar tvar acc { open = False }

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance (BankAccount tvar) = atomically $ do
    acc <- readTVar tvar
    if open acc
       then return (Just (balance acc))
       else return Nothing

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance (BankAccount tvar) amount = atomically $ do
    acc <- readTVar tvar
    if not (open acc)
       then return Nothing
       else do
           let newBal = balance acc + amount
           writeTVar tvar acc { balance = newBal }
           return (Just newBal)
