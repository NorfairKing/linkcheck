module LinkCheck
  ( linkCheck,
  )
where

import LinkCheck.OptParse

linkCheck :: IO ()
linkCheck = getSettings >>= print
