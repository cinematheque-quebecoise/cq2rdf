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
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Import
import Run
import RIO.Process
import Options.Applicative.Simple
import qualified Paths_cq2rdf

main :: IO ()
main = do
  (options, ()) <- simpleOptions
    $(simpleVersion Paths_cq2rdf.version)
    "cq2rdf  Copyright (C) 2020  Cinemathèque québécoise\nThis program comes with ABSOLUTELY NO WARRANTY.\nThis is free software, and you are welcome to redistribute it under certain conditions."
    "cq2rdf is a tool to convert the database of Cinemathèque québécoise in multple RDF formats."
    (Options
       <$> switch ( long "verbose"
                 <> short 'v'
                 <> help "Verbose output?"
                  )
       <*> parseCommand
       <*> strOption ( long "baseuri"
                    <> short 'b'
                    <> metavar "BASEURI"
                    <> help "RDF Base URI"
                     )
       <*> strOption ( long "sqlitedb"
                    <> short 's'
                    <> metavar "SQLITEDBFILE"
                    <> help "File path of the Sqlite database file"
                     )
       <*> strOption ( long "outputdir"
                    <> short 'o'
                    <> metavar "OUTPUTDIR"
                    <> help "Output directory of result"
                     )
    )
    empty
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          }
     in runRIO app run

parseCommand :: Parser Command
parseCommand = subparser $
     command "cinetv-to-rdf" (parseCinetvToRdfCommand `withInfo` "Convert CineTV to RDF")
  <> command "generate-void" (parseVoidGenerationCommand `withInfo` "Generate a VoID dataset from SPARQL endpoint")

parseCinetvToRdfCommand :: Parser Command
parseCinetvToRdfCommand = pure CinetvToRdf

parseVoidGenerationCommand :: Parser Command
parseVoidGenerationCommand = pure GenerateVoid

-- Where withInfo is just a convenience function to add --help support given
-- a parser and description
withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc
