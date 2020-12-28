{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Test.Hspec.Expectations.Extended
  ( shouldContainElems
  )
where

import           Import
import           Test.Hspec.Expectations (Expectation, shouldContain)

shouldContainElems :: (HasCallStack, Show a, Eq a) => [a] -> [a] -> Expectation
shouldContainElems actual expectedElems =
  forM_ expectedElems $ \element -> actual `shouldContain` [element]
