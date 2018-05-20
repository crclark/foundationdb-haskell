{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import FoundationDB.Experimental

main :: IO ()
main = withFoundationDB $
  withDatabase Nothing $ \case
    Left e -> error $ "error starting DB: " ++ show e
    Right db -> do
      _ <- runTransaction db $ set "foo12" "bar"
      v <- runTransaction db $ do f <- get "foo12"
                                  await f
      case v of
        Left e -> error $ "error getting test val: " ++ show e
        Right x -> print x
