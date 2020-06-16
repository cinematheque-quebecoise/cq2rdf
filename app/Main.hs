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
    "Header for command line arguments"
    "Program description, also for command line arguments"
    (Options
       <$> switch ( long "verbose"
                 <> short 'v'
                 <> help "Verbose output?"
                  )
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