module StdMain
  ( LogTIO, Overwrite(..)
  , lvlToDoMock, stdMain, stdMain_, stdMainSimple, stdMainNoDR
  , checkDirW, checkExtantsDups, checkFileW, checkFileWs, checkInputFile
  , checkInputFiles, checkMkdirs, checkOutputFiles, checkRunNICmds
  , checkRunNICmds', jsonParse, runNICmds, throwUsageErrors

  -- DEPRECATED
  , stdMainNoDR', stdMain''
  )
where

import Base1T  hiding  ( (∈) )

-- aeson -------------------------------

import Data.Aeson  ( FromJSON, eitherDecode' )

-- base --------------------------------

import Data.Bool            ( bool )
import Data.Foldable        ( Foldable )
import Data.Function        ( flip )
import Data.Maybe           ( catMaybes )
import Data.String          ( unwords, words )
import Data.Tuple           ( uncurry )

-- bytestring --------------------------

import qualified  Data.ByteString.Lazy  as  Lazy

-- containers-plus ---------------------

import ContainersPlus.Member  ( HasMember( (∈) ) )

-- exited ------------------------------

import qualified  Exited
import Exited  ( ToExitCode )

-- fpath -------------------------------

import FPath.AbsDir            ( AbsDir )
import FPath.AbsFile           ( AbsFile )
import FPath.AsFilePath        ( AsFilePath )
import FPath.File              ( FileAs )
import FPath.Dirname           ( dirname )
import FPath.Error.FPathError  ( AsFPathError )

-- lens --------------------------------

import Control.Lens.Getter  ( view )

-- log-plus ----------------------------

import Log              ( Log, logIOT
                        , logToFile', logFilter, logToStderr', stdRenderers )
import Log.LogEntry     ( LogEntry, attrs, mapPrefixDoc )
import Log.HasSeverity  ( severity )

-- logging-effect ----------------------

import Control.Monad.Log  ( LoggingT, MonadLog
                          , Severity( Debug, Informational, Notice, Warning ) )

-- mockio ------------------------------

import MockIO               ( DoMock( DoMock, NoMock ), HasDoMock )

-- mockio-log --------------------------

import MockIO.Log           ( MockIOClass, errIO', mkIOL )
import MockIO.IOClass       ( HasIOClass, ioClass )
import MockIO.RenderDoMock  ( renderWithDoMock )

-- mockio-plus -------------------------

import MockIO.Directory              ( mkdir )
import MockIO.File                   ( AccessMode( ACCESS_W, ACCESS_WX )
                                     , FExists( FExists, NoFExists )
                                     , fexists, fexists', lfexists, lfexists'
                                     )
import MockIO.FStat                  ( access, stat )
import MockIO.Process                ( ꙩ )
import MockIO.Process.MLCmdSpec      ( MLCmdSpec )
import MockIO.Process.OutputDefault  ( OutputDefault )

-- monadio-plus ------------------------

import MonadIO.Base                   ( getArgs )
import MonadIO.Error.CreateProcError  ( AsCreateProcError )
import MonadIO.Error.ProcExitError    ( AsProcExitError )
import MonadIO.File                   ( FileOpenMode( FileW )
                                      , FileType( Directory )
                                      , HEncoding( UTF8 )
                                      , fileWritable, ftype, withFile
                                      )
import MonadIO.NamedHandle            ( handle )
import MonadIO.Process.ExitStatus     ( ExitStatus, HasExitStatus( exitVal ) )
import MonadIO.Process.OutputHandles  ( OutputHandles )
import MonadIO.Process.MakeProc       ( MakeProc )
import MonadIO.Process.ToMaybeTexts   ( ToMaybeTexts )

-- mtl ---------------------------------

import Control.Monad.Reader  ( runReaderT )

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

import qualified Prettyprinter  as  PPDoc

-- text --------------------------------

import Data.Text  ( pack )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import StdMain.StdOptions      ( DryRunLevel, HasDryRunLevel( dryRunLevel )
                               , StdOptions
                               , callstackOnError, dryRunNum, options
                               , parseStdOptions, profCallstackOnError
                               )
import StdMain.ProcOutputParseError
                               ( AsProcOutputParseError
                               , throwAsProcOutputParseError )
import StdMain.UsageError      ( AsUsageError, UsageFPProcIOError, throwUsage )
import StdMain.VerboseOptions  ( ShowIOCs( DoShowIOCs )
                               , csopt, ioClassFilter, logFile, showIOCs
                               , unLogFile, verboseDesc, verboseOptions
                               )

--------------------------------------------------------------------------------

data Overwrite = Overwrite | NoOverwrite
  deriving (Eq,Show)

------------------------------------------------------------

type LogTIO ω ε = LoggingT (Log ω) (ExceptT ε IO)
-- type LogTIOM ε  = LoggingT (Log MockIOClass) (ExceptT ε IO)
type LogTIOM ε  = LogTIO MockIOClass ε

------------------------------------------------------------

{- | Disentangle dry-run level, options from a `StdOptions`. -}
drOpts ∷ StdOptions ν ρ → (DryRunLevel ν, ρ)
drOpts o = (o ⊣ dryRunLevel, o ⊣ options)

----------------------------------------

{- | Execute some logging IO, which has access to a dry-run level (parsed
     from arguments).  A parser is used to parse those arguments from input. -}
stdMain_ ∷ ∀ ε ρ σ ω ν μ .
           (MonadIO μ, Exception ε, Printable ε, AsUsageError ε, AsIOError ε,
            HasCallstack ε, ToExitCode σ, HasIOClass ω, HasDoMock ω, Default ω,
            HasCallStack) ⇒
           Natty ν                         -- ^ maximum `DryRun` level
         → 𝕋                               -- ^ program synopsis
         → Parser ρ                        -- ^ options parser
         → (StdOptions ν ρ → LogTIO ω ε σ) -- ^ program
         → [𝕊]                             -- ^ args to parse (e.g., cmdline)
         → μ ()
stdMain_ n desc p io args = do
  let io' = \ o → logIOT Debug ([fmt|cmdline args: %L|] args) ⪼ io o

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
                                          (unLogFile logfn)
                                          (logIOToFile io' o ∘ view handle)

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
              → (DoMock → ρ → (LogTIOM UsageFPProcIOError) σ)
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

----------------------------------------

----------------------------------------

{- | Check if we can write to a dir (which must exist); a list of dirs to treat
     as accessible (e.g., "we're going to create these (or die trying)") may be
     passed.
 -}
checkDirW ∷ (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ,
             Default ω, HasIOClass ω, HasDoMock ω, MonadLog (Log ω) μ) ⇒
            [AbsDir] → AbsDir → μ (𝕄 𝕋)
checkDirW ds d = do
  let accessE 𝕹     = if d ∈ ds
                      then return 𝕹
                      else return ∘ 𝕵 $ [fmt|No such dir: '%T'|] d
      accessE (𝕵 𝕿) = stat Informational 𝕹 d NoMock ≫ \ case
                         𝕹    → return ∘ 𝕵 $ [fmt|dir disappeared: '%T'|] d
                         𝕵 st → case ftype st of
                                  Directory → return 𝕹
                                  _         →
                                    return ∘ 𝕵 $ [fmt|not a dir: '%T'|] d
      accessE (𝕵 𝕱) = return ∘ 𝕵 $ [fmt|Cannot write to dir: '%T'|] d
   in access Informational ACCESS_WX 𝕹 d NoMock ≫ accessE

----------------------------------------

{- | Check a list of files; for each file check if it is duplicated elsewhere in
     the list; if it is extant and `overwrite` is `NoOverwrite`; or if it is
     extant, `overwrite` is `Overwrite`, but the file is not writable. -}
checkExtantsDups ∷ (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ,
                    MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω,
                    HasCallStack, Foldable φ) ⇒
                   Overwrite → φ AbsFile → μ [𝕋]
checkExtantsDups overwrite =
  fst ⩺ foldM check_fn ([],[])
  where
    -- Accumulator is text errors, and output files seen so far (for detecting
    -- duplicates).
    check_fn ∷ (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ,
                HasCallStack, MonadLog (Log ω) μ, Default ω, HasIOClass ω,
                HasDoMock ω) ⇒
               ([𝕋],[AbsFile]) → AbsFile → μ ([𝕋],[AbsFile])
    check_fn (errs,fs) f =
      if f ∈ fs
      then let msg = [fmt|duplicate output file: '%T'|] f
            in return (msg:errs,fs)
      else -- We use `lfexists'` here so that (a) /etc/passwd/ will return
           -- `FExists` if /etc/passwd exists; and (b) /foo will return
           -- `FExists` if /foo exists and is a symlink (even a dangling one).
           lfexists' Informational FExists f NoMock ≫ \ case
             FExists   → if Overwrite ≡ overwrite
                         then access Informational ACCESS_W 𝕹 f NoMock ≫ \ case
                                𝕹 → -- We warn directly, rather than returning
                                    -- an error, because it's a warning and
                                    -- probably shouldn't cause a failure.
                                    do let msg =
                                             ю [ "file disappeared while "
                                               ,  [fmtT|checking: '%T'|] f ]
                                       mkIOL Warning def msg () (return ())
                                             NoMock
                                       return (errs,f:fs)
                                𝕵 𝕱 → let msg = ю [ "output file is not "
                                                  , [fmt|writable: '%T'|] f ]
                                       in return (msg:errs,f:fs)
                                𝕵 𝕿 → return (errs,f:fs)
                         else let msg = [fmt|output file already exists: %T|] f
                               in return (msg:errs,f:fs)
             NoFExists → return (errs,f:fs)

----------------------------------------

{- | For each dir, check that either the dir exists and is writable or its
     parent exists and is writable (i.e., the dir could be created).  A list of
     errors is returned.
 -}
checkMkdirs ∷ ∀ ε ω μ .
              (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ,
               HasDoMock ω, HasIOClass ω, Default ω, MonadLog (Log ω) μ) ⇒
              [AbsDir] → μ [𝕋]
checkMkdirs dirs =
  let go d = access Informational ACCESS_WX 𝕹 d NoMock ≫ \ case
               𝕵 𝕿 → return 𝕹
               𝕵 𝕱 → return ∘ 𝕵 $ [fmt|'%T' is not writable|] d
               𝕹   → do
                 let p = d ⊣ dirname
                 access Informational ACCESS_WX 𝕹 p NoMock ≫ \ case
                   𝕹 → return ∘ 𝕵 $ [fmt|neither '%T' nor '%T' exist|] d p
                   𝕵 𝕿 → return 𝕹
                   𝕵 𝕱 → return ∘ 𝕵 $ [fmt|no '%T', and %T is not writable|] d p
   in catMaybes ⊳ forM dirs go

----------------------------------------

{- | Check for the writability of a file; specifically, the file must either
     exist, the overwrite flag be Overwrite, and the file be writable; or else
     the file not exist, and its parent directory either exist and be writable
     or else be cited in `make_dirs`.
 -}
checkFileW ∷ ∀ ε ω μ .
             (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ,
              HasDoMock ω, HasIOClass ω, Default ω, MonadLog (Log ω) μ) ⇒
             [AbsDir] → Overwrite → AbsFile → μ (𝕄 𝕋)
checkFileW make_dirs overwrite fn = do
  fexists Informational NoFExists fn NoMock ≫ \ case
    FExists   →
      case overwrite of
        NoOverwrite → return ∘ 𝕵 $ [fmtT|Not overwriting extant file '%T'|] fn
        Overwrite   → access Informational ACCESS_W (𝕵 𝕿) fn NoMock ≫ \ case
                        𝕹   → return ∘ 𝕵 $ [fmtT|File '%T' disappeared|] fn
                        𝕵 𝕿 → return 𝕹
                        𝕵 𝕱 → return ∘ 𝕵 $ [fmt|File '%T' is not writable|] fn
    NoFExists → let d = fn ⊣ dirname
                    pfx = (([fmt|Cannot output '%T': |] fn ⊕) ⊳)
                 in checkDirW make_dirs d ≫ return ∘ pfx

--------------------

checkFileWs ∷ ∀ ε ω μ .
              (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ,
               HasDoMock ω, HasIOClass ω, Default ω, MonadLog (Log ω) μ) ⇒
              [AbsDir] → Overwrite → [AbsFile] → μ [𝕋]
checkFileWs make_dirs overwrite fns =
   catMaybes ⊳ mapM (checkFileW make_dirs overwrite) fns

----------------------------------------

{- | Given a list of error messages; if the list is non-empty, emit each msg
     and die with a given usage error. -}

throwUsageErrors ∷ ∀ ε μ .
                   (MonadIO μ, MonadLog (Log MockIOClass) μ,
                    AsUsageError ε, MonadError ε μ) ⇒
                   DoMock → 𝕋 → [𝕋] → μ ()
throwUsageErrors _        _  []   = return ()
throwUsageErrors do_mock msg errs = do
    forM_ errs errIO'
    when (NoMock ≡ do_mock) $ throwUsage msg

----------------------------------------

checkInputFile ∷ ∀ ε ψ ω μ .
                 (MonadIO μ, FileAs ψ, AsFilePath ψ, Printable ψ,
                  AsIOError ε, Printable ε, HasCallStack, MonadError ε μ,
                  Default ω, HasIOClass ω, HasDoMock ω, MonadLog (Log ω) μ) ⇒
                 ψ → μ (𝕄 𝕋)
checkInputFile input =
  bool 𝕹 (𝕵 $ [fmt|No such input file: '%T'|] input) ∘ (≢ FExists) ⊳
    lfexists Debug FExists input NoMock

--------------------

{- | Return a text message for every non-extant file. -}
checkInputFiles ∷ ∀ ε ψ ω μ .
                  (MonadIO μ, FileAs ψ, AsFilePath ψ, Printable ψ,
                   AsIOError ε, Printable ε, HasCallStack, MonadError ε μ,
                   Default ω, HasIOClass ω, HasDoMock ω, MonadLog (Log ω) μ) ⇒
                  [ψ] → μ [𝕋]
checkInputFiles is = catMaybes ⊳ mapM checkInputFile is

----------------------------------------

{- | Check that a list of files contains no duplicates, and no extant files.

     Each of `fns` is checked for writability.
     -) If `overwrite` is `NoOverwrite`, then each `fns` must not exist but be a
        (potential) member of a writable directory, or an element of
        `make_dirs`.
     -) If `overwrite` is `Overwrite`, then a member of `fns` may exist but must
        itself be writable.
     -) Elements of `fns` may not be duplicated.

     Each of `make_dirs` is checked that either it or its parent exists and is
     writable.
-}
checkOutputFiles ∷ (MonadIO μ, MonadLog (Log MockIOClass) μ,
                    AsIOError ε, AsUsageError ε, Printable ε, MonadError ε μ) ⇒
                   [AbsFile] → [AbsDir] → Overwrite → μ [𝕋]
checkOutputFiles fns make_dirs overwrite = do
  access_errs ← checkFileWs make_dirs overwrite fns
  mkdir_errs  ← checkMkdirs make_dirs
  file_errs   ← checkExtantsDups overwrite fns

  return $ ю [ access_errs, file_errs, mkdir_errs ]

----------------------------------------

{- | Run a list of external processes that take nothing on stdin. -}
runNICmds ∷ ∀ ε ξ ζ μ .
            (MonadIO μ, HasCallStack,
             AsProcExitError ε, AsFPathError ε, AsCreateProcError ε,
             AsIOError ε, Printable ε, MonadError ε μ,
             ToMaybeTexts ξ, OutputDefault ξ, OutputHandles ζ ξ, MakeProc ζ,
             MonadLog (Log MockIOClass) μ) ⇒
            [DoMock → MLCmdSpec ξ] → DoMock → μ [(ExitStatus,ξ)]

runNICmds cmds do_mock =
  flip runReaderT do_mock $
    (fmap (first $ view exitVal)) ⊳ forM cmds (\ cmd → ꙩ (cmd do_mock))

----------------------------------------

{- | Run a list of external processes that take nothing on stdin; checking
     outputs before execution.  See `checkOutputFiles` for checks made.
     Non-extant elements of `make_dirs` will be created (mode 0755).
 -}
checkRunNICmds ∷ ∀ ε ζ ξ μ .
                 (MonadIO μ, HasCallStack,
                  AsProcExitError ε, AsFPathError ε, AsCreateProcError ε,
                  AsUsageError ε, AsIOError ε, Printable ε, MonadError ε μ,
                  ToMaybeTexts ξ, OutputDefault ξ, OutputHandles ζ ξ,MakeProc ζ,
                  MonadLog (Log MockIOClass) μ) ⇒
                 Overwrite → [DoMock → MLCmdSpec ξ] → [AbsFile] → [AbsDir]
               → DoMock → μ [(ExitStatus,ξ)]
checkRunNICmds overwrite cmds output_files make_dirs do_mock = do
  errs ← checkOutputFiles output_files make_dirs overwrite
  throwUsageErrors do_mock "file create error" errs
  let maybeMkdir d = do
        ex ← fexists' Informational NoFExists d NoMock
        when (NoFExists ≡ ex) $ mkdir Notice d 0755 do_mock
  forM_ make_dirs maybeMkdir
  runNICmds cmds do_mock

--------------------

{- | Like `checkRunNICmds`, but all output & exit statuses are discarded. -}
checkRunNICmds' ∷ ∀ ε ζ ξ μ .
                 (MonadIO μ, HasCallStack,
                  AsProcExitError ε, AsFPathError ε, AsCreateProcError ε,
                  AsUsageError ε, AsIOError ε, Printable ε, MonadError ε μ,
                  ToMaybeTexts ξ, OutputDefault ξ, OutputHandles ζ ξ,MakeProc ζ,
                  MonadLog (Log MockIOClass) μ) ⇒
                 Overwrite → [DoMock → MLCmdSpec ξ] → [AbsFile] → [AbsDir]
               → DoMock → μ ()
checkRunNICmds' overwrite cmds output_files make_dirs do_mock = do
  _  ← checkRunNICmds overwrite cmds output_files make_dirs do_mock
  return ()

----------------------------------------

jsonParse ∷ ∀ ε α η . (AsProcOutputParseError ε, MonadError ε η, FromJSON α) ⇒
            Lazy.ByteString → η α
jsonParse bs =
  either (throwAsProcOutputParseError ∘ pack) return $ eitherDecode' bs

-- that's all, folks! ----------------------------------------------------------
