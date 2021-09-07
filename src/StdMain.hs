module StdMain
  ( LogTIO, lvlToDoMock, stdMain, stdMain_, stdMainSimple, stdMainNoDR
  -- DEPRECATED
  , stdMainNoDR', stdMain''
  )
where

-- base --------------------------------

import Control.Applicative  ( pure )
import Control.Exception    ( Exception )
import Data.Bifunctor       ( first )
import Data.Function        ( ($) )
import Data.Ord             ( (<) )
import Data.String          ( unwords, words )
import Data.Tuple           ( uncurry )
import System.IO            ( IO )
import Text.Show            ( Show( show ) )

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

-- mockio-log --------------------------

import MockIO.Log           ( MockIOClass )
import MockIO.IOClass       ( HasIOClass, (∈), ioClass )
import MockIO.RenderDoMock  ( renderWithDoMock )

-- monaderror-io -----------------------

import MonadError           ( ѥ )
import MonadError.IO.Error  ( AsIOError )

-- monadio-plus ------------------------

import MonadIO       ( MonadIO, liftIO )
import MonadIO.Base  ( getArgs )
import MonadIO.File  ( FileOpenMode( FileW ), HEncoding( UTF8 )
                     , fileWritable, withFile )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊴) )
import Data.MoreUnicode.Either       ( pattern 𝕷, pattern 𝕽 )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Lens         ( (⊣) )
import Data.MoreUnicode.Maybe        ( pattern 𝕵, pattern 𝕹 )
import Data.MoreUnicode.Monad        ( (≫) )
import Data.MoreUnicode.Monoid       ( ю )
import Data.MoreUnicode.String       ( 𝕊 )
import Data.MoreUnicode.Text         ( 𝕋 )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, throwError )

-- natural-plus ------------------------

import Natural  ( Natty, One, none, one, count )

-- optparse-applicative ----------------

import Options.Applicative  ( Parser, footerDoc, helper, progDesc )
import Options.Applicative.Help.Pretty  ( Doc
                                        , (<+>)
                                        , align, empty, fillBreak, fillSep
                                        , indent, string, text
                                        , vcat
                                        )

-- optparse-plus -----------------------

import OptParsePlus  ( parseOpts_ )

-- prettyprinter -----------------------

import qualified Data.Text.Prettyprint.Doc  as  PPDoc

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import StdMain.StdOptions      ( DryRunLevel, HasDryRunLevel( dryRunLevel )
                               , StdOptions
                               , callstackOnError, dryRunNum, options
                               , parseStdOptions, profCallstackOnError
                               )
import StdMain.UsageError      ( AsUsageError, UsageIOError, throwUsage )
import StdMain.VerboseOptions  ( ShowIOCs( DoShowIOCs )
                               , csopt, ioClassFilter, logFile, showIOCs
                               , unLogFile, verboseDesc, verboseOptions
                               )

--------------------------------------------------------------------------------

type LogTIO ω ε = LoggingT (Log ω) (ExceptT ε IO)
type LogTIOM ε  = LoggingT (Log MockIOClass) (ExceptT ε IO)

------------------------------------------------------------

{- | Disentangle dry-run level, options from a `StdOptions`. -}
drOpts ∷ StdOptions ν ρ → (DryRunLevel ν, ρ)
drOpts o = (o ⊣ dryRunLevel, o ⊣ options)

----------------------------------------

{- | Execute some logging IO, which has access to a dry-run level (parsed
     from arguments).  A parser is used to parse those arguments from input. -}
stdMain_ ∷ ∀ ε ρ σ ω ν μ .
           (MonadIO μ, Exception ε, Printable ε, AsUsageError ε, AsIOError ε,
            HasCallstack ε, ToExitCode σ, HasIOClass ω, HasDoMock ω,
            HasCallStack) ⇒
           Natty ν                         -- ^ maximum `DryRun` level
         → 𝕋                               -- ^ program synopsis
         → Parser ρ                        -- ^ options parser
         → (StdOptions ν ρ → LogTIO ω ε σ) -- ^ program
         → [𝕊]                             -- ^ args to parse (e.g., cmdline)
         → μ ()
stdMain_ n desc p io args = do
  let optionDesc ∷ 𝕊 → [𝕊] → Doc
      optionDesc name descn =
        let para = fillSep $ text ⊳ (words $ unwords descn)
         in indent 2 (fillBreak 14 (string name) <+> align para)
      optionDesc' ∷ 𝕊 → Doc → Doc
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
  o ← parseOpts_ args (progDesc (toString desc) ⊕ footerDoc (𝕵 footerDesc))
                (parseStdOptions n p ⊴ helper)
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

----------------------------------------

lvlToDoMock ∷ HasDryRunLevel One ν ⇒ ν → DoMock
lvlToDoMock l = if 0 < dryRunNum l then DoMock else NoMock

{- | `stdMain_`  with `ω` fixed to `MockIOClass` (i.e., logging with
     MockIOClass), `ν` fixed to `one` (i.e., a single dry-run level); and that
     dry-run level is translated to a `DoMock`.
 -}
stdMain ∷ ∀ ε ρ σ μ .
          (MonadIO μ, Exception ε, Printable ε, AsUsageError ε, AsIOError ε,
           HasCallstack ε, ToExitCode σ) ⇒
          𝕋                          -- ^ program description
        → Parser ρ                   -- ^ options parser
        → (DoMock → ρ → LogTIOM ε σ) -- ^ main program
        → [𝕊]                        -- ^ arguments to parse
        → μ ()
stdMain desc p io =
  stdMain_ one desc p (\ o → uncurry io (first lvlToDoMock $ drOpts o))

----------------------------------------

{- | Version of `stdMain`, with more simple type; the args are taken directly
     from the cmdline; where the error is specifically a `UsageIOError`, and
     there is a single dry-run level which is translated to DoMock/NoMock;
     intended for simple IO programs.
 -}
stdMainSimple ∷ ∀ ρ σ μ . (MonadIO μ, ToExitCode σ) ⇒
                𝕋
              → Parser ρ
              → (DoMock → ρ → (LogTIOM UsageIOError) σ)
              → μ ()
stdMainSimple desc parser io = getArgs ≫ stdMain desc parser io

----------------------------------------

{- | Like `stdMain`, but with no `DryRun` option and `ω` fixed to
     `MockIOClass`. -}
stdMainNoDR ∷ ∀ ε ρ σ μ .
              (MonadIO μ, Exception ε, Printable ε, AsUsageError ε, AsIOError ε,
               HasCallstack ε, ToExitCode σ) ⇒
              𝕋
            → Parser ρ
            → (ρ → LogTIOM ε σ)
            → [𝕊]
            → μ ()
stdMainNoDR desc p io = stdMain_ none desc p (\ o → io (o ⊣ options))

-- deprecated functions ------------------------------------

{- | `stdMain_` with `ω` fixed to `MockIOClass` (i.e., logging with
      MockIOClass) and `ν` fixed to `one` (i.e., a single dry-run level). -}
stdMain'' ∷ ∀ ε ρ σ μ .
          (MonadIO μ, Exception ε, Printable ε, AsUsageError ε, AsIOError ε,
           HasCallstack ε, ToExitCode σ) ⇒
          𝕋
        → Parser ρ
        → (DryRunLevel One → ρ → LogTIOM ε σ)
        → μ ()
{-# DEPRECATED stdMain'' "use getArgs ≫ stdMain" #-}
stdMain'' desc parser io =
  liftIO getArgs ≫ stdMain_ one desc parser (\ o → uncurry io (drOpts o))

--------------------

stdMainNoDR' ∷ ∀ ε ρ σ μ .
              (MonadIO μ, Exception ε, Printable ε, AsUsageError ε, AsIOError ε,
               HasCallstack ε, ToExitCode σ) ⇒
              𝕋
            → Parser ρ
            → (ρ → LogTIOM ε σ)
            → μ ()
{-# DEPRECATED stdMainNoDR' "use getArgs ≫ stdMainNoDR" #-}
stdMainNoDR' desc parser io = getArgs ≫ stdMainNoDR desc parser io

-- that's all, folks! ----------------------------------------------------------
