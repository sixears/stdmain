{-# LANGUAGE UnicodeSyntax #-}
{- | Tools for working with options. -}
module StdMain.OptionsTools
  ( fileToAbsNoOverwrite
  , fileToAbsUE
  , fpathIOErrorToAsUsageIOError
  ) where

import Base1T

-- fpath -------------------------------

import FPath.AbsFile          ( AbsFile )
import FPath.Error.FPathError ( FPathIOError, fpathIOErrorEither )

-- fstat -------------------------------

import FStat ( FileType(CharacterDevice, NamedPipe, Socket), ftype )

-- log-plus ----------------------------

import Log ( Log )

-- logging-effect ----------------------

import Control.Monad.Log ( MonadLog, Severity(Informational) )

-- mockio ------------------------------

import MockIO         ( DoMock(NoMock), HasDoMock )
import MockIO.IOClass ( HasIOClass )

-- monadio-plus ------------------------

import MockIO.FStat  ( stat )
import MonadIO.FPath ( PResolvable, pResolve )

-- monaderror-io -----------------------

import MonadError ( mapMError' )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import StdMain.UsageError ( AsUsageError, throwUsage, usageError )

--------------------------------------------------------------------------------

{- | Convert an `FPathIOError` to a `UsageIOError` or similar. -}
fpathIOErrorToAsUsageIOError ∷ (AsIOError ε, AsUsageError ε, HasCallStack) ⇒
                               FPathIOError → ε
fpathIOErrorToAsUsageIOError e =
  case fpathIOErrorEither e of
    Left  ioE → _IOError # ioE
    Right fpE → usageError (toText fpE)

----------------------------------------

{- | Convert a `File` (e.g., as provided on the cmdline) to an `AbsFile` (using
     the Cwd); throwing a `UsageIOError` or similar in case of failure.
-}
fileToAbsUE ∷ (MonadIO μ,
               AsIOError ε, AsUsageError ε, HasCallStack, MonadError ε μ,
               Printable τ, PResolvable α) ⇒
              τ → μ α
fileToAbsUE = mapMError' fpathIOErrorToAsUsageIOError ∘ pResolve

----------------------------------------

{- | Convert a `File` (e.g., as provided on the cmdline) to an `AbsFile` (using
     the cwd) suitable for writing to without overwriting; throwing a
     `UsageIOError` or similar in case of failure, including
     if the file already exists but is neither a CharacterDevice, NamedPipe, nor
     Socket.  Symlinks are dereferenced.
-}
fileToAbsNoOverwrite ∷ ∀ τ ε ω μ.
                       (MonadIO μ, Printable τ,
                        Default ω, HasIOClass ω, HasDoMock ω,
                        MonadLog (Log ω) μ,
                        Printable ε, AsIOError ε, AsUsageError ε,
                        MonadError ε μ, HasCallStack) ⇒
                       τ → μ AbsFile

fileToAbsNoOverwrite f = do
  a ← fileToAbsUE f
  -- We use `stat` rather than `lstat` because:
  --  -) If it's link to a char device: use it ("overwrite")
  --  -) If it's a dangling symlink: use it (write, presumably creating the
  --     file)
  -- Passive action: run in dry-run mode, too - this isn't writing (or even
  -- opening) the file, it's just checking whether we should (in non-dry-run
  -- mode).
  st ← stat Informational Nothing a NoMock
  case ftype ⊳ st of
    Just CharacterDevice → return a
    Just NamedPipe       → return a
    Just Socket          → return a
    Nothing              → return a
    _                    → throwUsage $ [fmtT|not overwriting extant '%T'|] f

-- that's all, folks! ----------------------------------------------------------
