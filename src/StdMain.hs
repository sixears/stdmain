module StdMain
  ( LogTIO, stdMain, stdMainSimple, stdMain', stdMain'' )
where

-- base --------------------------------

import Control.Applicative     ( pure )
import Control.Exception       ( Exception )
import Control.Monad.IO.Class  ( MonadIO )
import Data.Function           ( ($) )
import Data.String             ( String, unwords, words )
import System.IO               ( IO )
import Text.Show               ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )
import Data.Ord.Unicode       ( (≤) )
import GHC.Stack              ( HasCallStack )

-- data-textual ------------------------

import Data.Textual  ( Printable, toString, toText )

-- exited ------------------------------

import qualified  Exited
import Exited  ( ToExitCode )

-- has-callstack -----------------------

import HasCallstack  ( HasCallstack )

-- log-plus ----------------------------

import Log              ( Log
                        , logToFile', logFilter, logToStderr', stdRenderers )
import Log.LogEntry     ( LogEntry, attrs, mapPrefixDoc )
import Log.HasSeverity  ( severity )

-- logging-effect ----------------------

import Control.Monad.Log  ( LoggingT )

-- mockio ------------------------------

import MockIO               ( DoMock( DoMock, NoMock ), HasDoMock )
import MockIO.Log           ( MockIOClass )
import MockIO.IOClass       ( HasIOClass, (∈), ioClass )
import MockIO.RenderDoMock  ( renderWithDoMock )

-- monaderror-io -----------------------

import MonadError           ( ѥ )
import MonadError.IO.Error  ( AsIOError )

-- monadio-plus ------------------------

import MonadIO.File  ( FileOpenMode( FileW ), HEncoding( UTF8 )
                     , fileWritable, withFile )

-- more-unicode ------------------------

import Data.MoreUnicode.Either   ( pattern 𝕷, pattern 𝕽 )
import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Lens     ( (⊣) )
import Data.MoreUnicode.Maybe    ( pattern 𝕵, pattern 𝕹 )
import Data.MoreUnicode.Monad    ( (≫) )
import Data.MoreUnicode.Monoid   ( ю )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, throwError )
import Control.Monad.Reader  ( ReaderT, runReaderT )
import Control.Monad.Trans   ( lift )

-- natural-plus ------------------------

import Natural  ( Natty, One, one, count )

-- optparse-applicative ----------------

import Options.Applicative  ( Parser, footerDoc, progDesc )
import Options.Applicative.Help.Pretty  ( Doc
                                        , (<+>)
                                        , align, empty, fillBreak, fillSep
                                        , indent, string, text
                                        , vcat
                                        )

-- optparse-plus -----------------------

import OptParsePlus  ( parseOpts )

-- prettyprinter -----------------------

import qualified Data.Text.Prettyprint.Doc  as  PPDoc

-- text --------------------------------

import Data.Text  ( Text )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import StdMain.StdOptions      ( DryRunLevel, HasDryRunLevel( dryRunLevel )
                               , StdOptions
                               , callstackOnError, ifDryRun, options
                               , parseStdOptions, profCallstackOnError
                               )
import StdMain.UsageError      ( AsUsageError, UsageIOError, throwUsage )
import StdMain.VerboseOptions  ( ShowIOCs( DoShowIOCs )
                               , csopt, ioClassFilter, logFile, showIOCs
                               , unLogFile, verboseDesc, verboseOptions
                               )

--------------------------------------------------------------------------------

{- | Like `stdMain`, but gives the incoming `io` full access to the `StdOptions`
     object. -}

stdMain_ ∷ ∀ ε α σ ω ν μ .
           (MonadIO μ, Exception ε, Printable ε, AsUsageError ε, AsIOError ε,
            HasCallstack ε, ToExitCode σ, HasIOClass ω, HasDoMock ω,
            HasCallStack) ⇒
           Natty ν
         → Text
         → Parser α
         → (StdOptions ν α → LoggingT (Log ω) (ExceptT ε IO) σ)
         → μ ()
stdMain_ n desc p io = do
  let optionDesc ∷ String → [String] → Doc
      optionDesc name descn =
        let para = fillSep $ text ⊳ (words $ unwords descn)
         in indent 2 (fillBreak 14 (string name) <+> align para)
      optionDesc' ∷ String → Doc → Doc
      optionDesc' name para =
        indent 2 (fillBreak 14 (string name) <+> align para)
      footerDesc ∷ Doc
      footerDesc = vcat ([ empty, string "Standard options:"
                         , optionDesc "-v" [ "Increase verbosity.  This may"
                                           , "be used up to 3 times (which is"
                                           , "equivalent to --debug); and is"
                                           , "exclusive with --quiet,"
                                           , "--debug, and --verbose."
                                           ]

                         , optionDesc "--quiet" [ "Decrease verbosity.  This "
                                                , "may be used up to 4 times;"
                                                , "and is exclusive with -v,"
                                                , "--debug, and --verbose."
                                                ]

                         , optionDesc "--debug" [ "Set verbosity to maximum"
                                                , "(debug level).  This option "
                                                , "is exclusive with -v,"
                                                , "--quiet, and --verbose."
                                                ]
                         ] ⊕ case count n of
                               0 → []
                               1 → [ optionDesc "--dry-run"
                                                [ "Do not make any changes; "
                                                , "just pretend."
                                                ]
                                   ]
                               _ → [ optionDesc "--dry-run"
                                                [ "Do not make any changes; "
                                                , "just pretend.  May be used"
                                                , "up to ", show (count n)
                                                , " times."
                                                ]
                                   ]

                         ⊕ [ optionDesc' "--verbose=OPTS" verboseDesc
                           , optionDesc "--callstack-on-error / -!"
                                                [ "In case of error, show"
                                                , "a callstack of where the"
                                                , "error was created."
                                                ]
                           , optionDesc "--prof-callstack-on-error / -#"
                                                [ "In case of error, show"
                                                , "a profiler-generated"
                                                , "callstack of where the error"
                                                , "was created if one is"
                                                , "available."
                                                ]
                           ]
                        )
  o ← parseOpts 𝕹 (progDesc (toString desc) ⊕ footerDoc (𝕵 footerDesc))
                  (parseStdOptions n p)
  let vopts      = o ⊣ verboseOptions
      ioClasses  = vopts ⊣ ioClassFilter
      sevOpt     = o ⊣ severity
      renderers  = renderWithDoMock : stdRenderers (vopts ⊣ csopt)

      prefixIOC ∷ ∀ α β . HasIOClass α ⇒ LogEntry α → PPDoc.Doc β
      prefixIOC le =
        PPDoc.braces (PPDoc.pretty ∘ toText $ le ⊣ attrs∘ioClass) ⊕ PPDoc.space
      filters    = ю [ if vopts ⊣ showIOCs ≡ DoShowIOCs
                       then [ pure ∘ mapPrefixDoc prefixIOC ]
                       else []
                     , [ \ le → if le ⊣ severity ≤ sevOpt then [le] else []
                       , logFilter (\ le → (le ⊣ attrs ∘ ioClass) ∈ ioClasses)
                       ]
                     ]
      logIOToFile i o' h = logToFile' renderers filters h (i o')


  Exited.doMainCS (o ⊣ callstackOnError, o ⊣ profCallstackOnError) $
    case vopts ⊣ logFile of
      𝕹       → logToStderr' renderers filters (io o)
      𝕵 logfn → ѥ (fileWritable (unLogFile logfn)) ≫ \ case
                  𝕷 e     → throwError e
                  𝕽 (𝕵 e) → throwUsage $ "bad log file: " ⊕ e
                  𝕽 𝕹     → withFile UTF8 (FileW $ 𝕵 0640)
                                          (unLogFile logfn) (logIOToFile io o)


----------

{- | Execute the 'main' of a standard program with standard options that returns
     a toExitCode, that may throw exceptions; logging as requested by cmdline
     options.

     The `LoggingT (Log ω) (LoggingT (Log ω) (ExceptT ε IO)) α` is satisfied by,
     e.g.,
     `MonadLog (Log IOClass) μ, MonadIO μ, MonadError ε μ, AsUsageError ε) ⇒ μ α`
     though quite honestly, I couldn't say why the double `Logging`.
 -}
stdMain ∷ ∀ ε α σ ω ν μ .
          (MonadIO μ, Exception ε, Printable ε, AsUsageError ε, AsIOError ε,
           HasCallstack ε, ToExitCode σ, HasIOClass ω, HasDoMock ω) ⇒
          Natty ν
        → Text
        → Parser α
        → (DryRunLevel ν → α → LoggingT (Log ω) (ExceptT ε IO) σ)
        → μ ()
stdMain n desc p io =
  stdMain_ n desc p (\ o → io (o ⊣ dryRunLevel) (o ⊣ options))

type LogTIO ω ε = (LoggingT (Log ω) (ExceptT ε IO))

stdMainx ∷ ∀ ε α σ ω ν μ .
          (MonadIO μ, Exception ε, Printable ε, AsUsageError ε, AsIOError ε,
           HasCallstack ε, ToExitCode σ, HasIOClass ω, HasDoMock ω) ⇒
          Natty ν
        → Text
        → Parser α
        → (α → ReaderT (DryRunLevel ν) (LogTIO ω ε) σ)
        → μ ()
stdMainx n desc p io =
  stdMain_ n desc p (\ o → runReaderT (io (o ⊣ options)) (o ⊣ dryRunLevel))

----------

{- | Version of `stdMain`, with more simple type; where the error is
     specifically a `UsageIOError`, and there is a single dry-run level which is
     translated to DoMock/NoMock; intended for simple IO programs.
 -}
stdMainSimple ∷ ∀ ρ σ μ . (MonadIO μ, ToExitCode σ) ⇒
                Text
              → Parser ρ
              → (DoMock → ρ → (LogTIO MockIOClass UsageIOError) σ)
              → μ ()
stdMainSimple desc parser io =
  let go opts = do
        mock ← ifDryRun DoMock NoMock
        lift $ io mock opts
   in stdMainx one desc parser go

----------------------------------------

{- | `stdMain` with `ω` fixed to `MockIOClass` (i.e., logging with
     MockIOClass). -}
stdMain' ∷ ∀ ε α σ ν μ .
          (MonadIO μ, Exception ε, Printable ε, AsUsageError ε, AsIOError ε,
           HasCallstack ε, ToExitCode σ) ⇒
          Natty ν
        → Text
        → Parser α
        → (DryRunLevel ν → α → LoggingT (Log MockIOClass) (ExceptT ε IO) σ)
        → μ ()
stdMain' = stdMain

----------------------------------------

{- | `stdMain'` with `ν` fixed to `one` (i.e., a single dry-run level). -}
stdMain'' ∷ ∀ ε α σ μ .
          (MonadIO μ, Exception ε, Printable ε, AsUsageError ε, AsIOError ε,
           HasCallstack ε, ToExitCode σ) ⇒
          Text
        → Parser α
        → (DryRunLevel One → α → LoggingT (Log MockIOClass) (ExceptT ε IO) σ)
        → μ ()
stdMain'' = stdMain' one

-- that's all, folks! ----------------------------------------------------------
