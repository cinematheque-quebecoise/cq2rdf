-- This file is part of cq2rdf.

-- cq2rdf is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- cq2rdf is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with cq2rdf.  If not, see <https://www.gnu.org/licenses/>.

{-# LANGUAGE NoImplicitPrelude #-}
module Types where

import           RIO
import           RIO.Process

data Command = CinetvToRdf
             | GenerateVoid

-- | Command line arguments
data Options = Options
    { optionsVerbose    :: !Bool
    , optionsCommand :: !Command
    , optionsBaseUri    :: !Text
    , optionsSqlitePath :: !Text
    , optionsOutputDir  :: !Text
    }

data App = App
    { appLogFunc        :: !LogFunc
    , appProcessContext :: !ProcessContext
    , appOptions        :: !Options
    -- Add other app-specific configuration information here
    }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL =
    lens appProcessContext (\x y -> x { appProcessContext = y })
