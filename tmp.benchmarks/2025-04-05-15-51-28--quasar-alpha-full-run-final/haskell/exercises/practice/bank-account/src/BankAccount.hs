module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import Control.Concurrent.STM
import Control.Monad (when)

data BankAccount = BankAccount (TVar (Maybe Integer))

openAccount :: IO BankAccount
openAccount = do
    tvar <- newTVarIO (Just 0)
    return (BankAccount tvar)

closeAccount :: BankAccount -> IO ()
closeAccount (BankAccount tvar) = atomically $ writeTVar tvar Nothing

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance (BankAccount tvar) = atomically $ readTVar tvar

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance (BankAccount tvar) amount = atomically $ do
    mbalance <- readTVar tvar
    case mbalance of
        Nothing -> return Nothing
        Just balance -> do
            let newBalance = balance + amount
            writeTVar tvar (Just newBalance)
            return (Just newBalance)
