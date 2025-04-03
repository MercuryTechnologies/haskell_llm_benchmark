module SecretHandshake (handshake) where

import Data.Bits

handshake :: Int -> [String]
handshake n =
  let
    actions = []
    wink = if (n .&. 1) /= 0 then ["wink"] else []
    doubleBlink = if (n .&. 2) /= 0 then ["double blink"] else []
    closeEyes = if (n .&. 4) /= 0 then ["close your eyes"] else []
    jump = if (n .&. 8) /= 0 then ["jump"] else []
    reverseOrder = (n .&. 16) /= 0
    allActions = wink ++ doubleBlink ++ closeEyes ++ jump
  in
    if reverseOrder then reverse allActions else allActions
