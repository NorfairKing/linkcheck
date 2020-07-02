module LinkCheck
  ( linkCheck,
  )
where

import Conduit
import LinkCheck.OptParse

linkCheck :: IO ()
linkCheck = getSettings >>= print
