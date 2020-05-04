-- cq2rdf - Generates RDF file from Cinémathèque québécoise database.
-- Copyright (C) 2020  Cinémathèque québécoise

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{- |
   Module      : Model
   Copyright   : Copyright (C) 2020  Cinémathèque québécoise
   License     : GNU GPL, version 3 or above

   Maintainer  : Konstantinos Lambrou-Latreille <klambroulatreille@cinematheque.qc.ca>
   Stability   : alpha
-}
module Model where

import Import hiding ((^.))

import Data.Time.Clock
import Database.Persist.TH (share, mkPersist, sqlSettings, mkMigrate, persistFileWith)
import Database.Persist.Quasi (upperCaseSettings)

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
  $(persistFileWith upperCaseSettings "config/cinetv.persistentmodels")

