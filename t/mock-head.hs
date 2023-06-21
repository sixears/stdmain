{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE UnicodeSyntax     #-}

import Base1T

-- base --------------------------------

import qualified  System.IO

import Control.Applicative     ( optional )
import Data.List               ( take )
import System.IO               ( Handle, IOMode( WriteMode ), stdout )

-- fpath -------------------------------

import FPath.AbsFile    ( AbsFile )
import FPath.Parseable  ( readM )

-- log-plus ----------------------------

import Log  ( Log )

-- logging-effect ----------------------

import Control.Monad.Log  ( MonadLog, Severity( Informational, Notice ) )

-- optparse-applicative ----------------

import Options.Applicative  ( Parser
                            , help, long, metavar, option, short, strArgument )

-- std-main ----------------------------

import StdMain             ( stdMainSimple )
import StdMain.UsageError  ( AsUsageError )

-- text --------------------------------

import qualified  Data.Text.IO

import Data.Text     ( Text, lines, unlines, unpack )
import Data.Text.IO  ( hPutStr )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MockIO          ( DoMock( NoMock ) )
import MockIO.Log      ( MockIOClass, mkIOL )
import MockIO.IOClass  ( IOClass( IORead, IOWrite ) )

--------------------------------------------------------------------------------

-- a very simple version of 'head', for testing MockIO

data Options = Options { fileName  ∷ Text
                       , outputFile ∷ Maybe AbsFile
                       }
  deriving Show

parseOptions ∷ Parser Options
parseOptions = Options ⊳ strArgument (metavar "FILE")
                       ⊵ optional (option readM (ю [ long "output"
                                                   , short 'o'
                                                   , metavar "FILE"
                                                   , help "write output here"
                                                   ])
                                  )

main ∷ IO ()
main = -- XXX Tidy This Up
       -- add 'append' to log file options
       -- log rolling!
       -- add Logging to withFile
       -- cmd logging using showcmdforuser

       stdMainSimple ("simple 'head' re-implementation to test MockIO" ∷ 𝕋)
                     parseOptions go

go ∷ (MonadLog (Log MockIOClass) μ, MonadIO μ, MonadError ε μ, AsUsageError ε) ⇒
     DoMock → Options → μ ()
go mck opts = do
  let fn = fileName opts
  txt ← take 10 ∘ lines ⊳ readFile fn
  writeFile mck (outputFile opts) (unlines txt)

withFile ∷ MonadIO μ ⇒ AbsFile → IOMode → (Handle → IO ω) → μ ω
withFile fn mode = liftIO ∘ System.IO.withFile (toString fn) mode

{- | With a file opened for writing, or `stdout` if no file provided. -}
withWriteFile ∷ (MonadIO μ, MonadLog (Log MockIOClass) μ) ⇒
                DoMock → α → Maybe AbsFile → (Handle → IO α) → μ α
withWriteFile mck a fn io = do
  let fname  = maybe "-STDOUT-" toText fn
      logmsg = [fmtT|write %t|] fname
  case fn of
    Nothing  → liftIO $ io stdout
    Just wfn → mkIOL Notice IOWrite logmsg a (withFile wfn WriteMode io) mck

writeFile ∷ (MonadIO μ, MonadLog (Log MockIOClass) μ) ⇒
            DoMock → Maybe AbsFile → Text → μ ()
writeFile mck fnY txt = withWriteFile mck () fnY (\ h → hPutStr h txt)

readFile ∷ (MonadIO μ, MonadLog (Log MockIOClass) μ) ⇒ Text → μ Text
readFile fn = let logmsg = [fmtT|read %t|] fn
                  result = Data.Text.IO.readFile (unpack fn)
               in mkIOL Informational IORead logmsg "" result NoMock

-- that's all, folks! ----------------------------------------------------------
