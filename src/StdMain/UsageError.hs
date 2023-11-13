{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UnicodeSyntax  #-}

{-| @UsageError@ and many combined errors -}

module StdMain.UsageError
  ( AsUsageError(_UsageError)
  , UsageError
  , UsageFPIOTPError
  , UsageFPProcIOError
  , UsageFPathError
  , UsageFPathIOError
  , UsageIOError
  , UsageParseAesonFPPIOError
  , UsageParseFPProcIOError
  , readUsage
  , throwUsage
  , throwUsageT
  , usageError
  ) where

import Base1T

-- aeson-plus --------------------------

import Data.Aeson.Error ( AesonError, AsAesonError(_AesonError) )

-- base --------------------------------

import GHC.Generics ( Generic )
import Text.Read    ( Read, readMaybe )

-- deepseq -----------------------------

import Control.DeepSeq ( NFData )

-- fpath -------------------------------

import FPath.Error.FPathError ( AsFPathError(_FPathError), FPathError,
                                FPathIOError )

-- monaderror-io -----------------------

import MonadError.IO.Error ( IOError )

-- monadio-plus ------------------------

import MonadIO.Error.CreateProcError ( AsCreateProcError(_CreateProcError),
                                       CreateProcError )
import MonadIO.Error.ProcExitError   ( AsProcExitError(_ProcExitError),
                                       ProcExitError )

-- parsec-plus -------------------------

import ParsecPlus ( AsParseError(_ParseError), ParseError )

-- text --------------------------------

import Data.Text ( Text )

-- text-printer ------------------------

import Text.Printer qualified as P

-- textual-plus ------------------------

import TextualPlus.Error.TextualParseError ( AsTextualParseError(_TextualParseError),
                                             TextualParseError )

--------------------------------------------------------------------------------

{-| an error in cmdline calling args & options -}
data UsageError = UsageError { _txt       :: Text
                             , _callstack :: CallStack
                             }
  deriving (Generic, NFData, Show)

----------------------------------------

instance Exception UsageError

----------------------------------------

instance Eq UsageError where
  (UsageError a _) == (UsageError b _) = a == b

----------------------------------------

instance HasCallstack UsageError where
  callstack = lens _callstack (\ eu cs → eu { _callstack = cs })

----------------------------------------

{-| prisms including @UsageError -}
class AsUsageError ε where
  _UsageError ∷ Prism' ε UsageError

--------------------

instance AsUsageError UsageError where
  _UsageError = id

--------------------

instance Printable UsageError where
  print = P.text ∘ _txt

------------------------------------------------------------

{-| create an @AsUsageError@ from a @ToText@ -}
usageError ∷ ∀ τ ε . (AsUsageError ε, Printable τ, HasCallStack) ⇒ τ → ε
usageError t = _UsageError # UsageError (toText t) callStack

----------------------------------------

{-| throw an @AsUsageError@, given a @ToText@ -}
throwUsage ∷ ∀ τ ε ω η . (Printable τ, AsUsageError ε, MonadError ε η) ⇒ τ → η ω
throwUsage t = throwError $ usageError t

----------------------------------------

{-| throw an @AsUsageError@, given a @ToText@ -}
throwUsageT ∷ ∀ ε ω η . (AsUsageError ε, MonadError ε η) ⇒ 𝕋 → η ω
throwUsageT = throwUsage

----------------------------------------

{-| try to @readMaybe@ a @Printable@ to a @Read@ value; on failure, throw an
    @AsUsageError@ -}
readUsage ∷ ∀ τ ε ω η . (AsUsageError ε, MonadError ε η, Read ω, Printable τ) ⇒
            τ → η ω
readUsage s = let errMsg = [fmtT|failed to parse: '%T'|] s
               in maybe (throwUsage $ errMsg) return (readMaybe $ toString s)

------------------------------------------------------------

{-| combined @UsageError@ & @IOError@ -}
data UsageIOError = UIOE_USAGE_ERROR UsageError
                  | UIOE_IO_ERROR IOError
  deriving (Generic, NFData)

_UIOE_USAGE_ERROR ∷ Prism' UsageIOError UsageError
_UIOE_USAGE_ERROR = prism' (\ e → UIOE_USAGE_ERROR e)
                           (\ case UIOE_USAGE_ERROR e → 𝕵 e; _ → 𝕹)

_UIOE_IO_ERROR ∷ Prism' UsageIOError IOError
_UIOE_IO_ERROR = prism' (\ e → UIOE_IO_ERROR e)
                        (\ case UIOE_IO_ERROR e → 𝕵 e; _ → 𝕹)

--------------------

instance Exception UsageIOError

--------------------

instance Show UsageIOError where
  show (UIOE_USAGE_ERROR e) = show e
  show (UIOE_IO_ERROR    e) = show e

--------------------

instance AsUsageError UsageIOError where
  _UsageError = _UIOE_USAGE_ERROR

--------------------

instance AsIOError UsageIOError where
  _IOError = _UIOE_IO_ERROR

--------------------

instance Printable UsageIOError where
  print (UIOE_USAGE_ERROR e) = print e
  print (UIOE_IO_ERROR    e) = print e

--------------------

instance HasCallstack UsageIOError where
  callstack =
    let
      getter (UIOE_USAGE_ERROR e) = e ⊣ callstack
      getter (UIOE_IO_ERROR    e) = e ⊣ callstack
      setter (UIOE_USAGE_ERROR e) cs = UIOE_USAGE_ERROR (e & callstack ⊢ cs)
      setter (UIOE_IO_ERROR    e) cs = UIOE_IO_ERROR    (e & callstack ⊢ cs)
    in
      lens getter setter

------------------------------------------------------------

{-| combined @UsageError@ & @FPathError@ -}
data UsageFPathError = UFPE_USAGE_ERROR UsageError
                     | UFPE_FPATH_ERROR FPathError
  deriving (Eq, Generic, NFData)

_UFPE_USAGE_ERROR ∷ Prism' UsageFPathError UsageError
_UFPE_USAGE_ERROR = prism' (\ e → UFPE_USAGE_ERROR e)
                           (\ case UFPE_USAGE_ERROR e → 𝕵 e; _ → 𝕹)

_UFPE_FPATH_ERROR ∷ Prism' UsageFPathError FPathError
_UFPE_FPATH_ERROR = prism' (\ e → UFPE_FPATH_ERROR e)
                           (\ case UFPE_FPATH_ERROR e → 𝕵 e; _ → 𝕹)

--------------------

instance Exception UsageFPathError

--------------------

instance Show UsageFPathError where
  show (UFPE_USAGE_ERROR e) = show e
  show (UFPE_FPATH_ERROR e) = show e

--------------------

instance AsUsageError UsageFPathError where
  _UsageError = _UFPE_USAGE_ERROR

--------------------

instance AsFPathError UsageFPathError where
  _FPathError = _UFPE_FPATH_ERROR ∘ _FPathError

--------------------

instance Printable UsageFPathError where
  print (UFPE_USAGE_ERROR e)    = print e
  print (UFPE_FPATH_ERROR    e) = print e

--------------------

instance HasCallstack UsageFPathError where
  callstack =
    let
      getter (UFPE_USAGE_ERROR e) = e ⊣ callstack
      getter (UFPE_FPATH_ERROR e) = e ⊣ callstack
      setter (UFPE_USAGE_ERROR e) cs = UFPE_USAGE_ERROR (e & callstack ⊢ cs)
      setter (UFPE_FPATH_ERROR e) cs = UFPE_FPATH_ERROR (e & callstack ⊢ cs)
    in
      lens getter setter

------------------------------------------------------------

{-| combined @UsageError@, @IOError@ & @FPathError@ -}
data UsageFPathIOError = UFPIOE_USAGE_ERROR UsageError
                       | UFPIOE_FPATHIO_ERROR FPathIOError
  deriving (Eq, Generic, NFData)

_UFPIOE_USAGE_ERROR ∷ Prism' UsageFPathIOError UsageError
_UFPIOE_USAGE_ERROR = prism' (\ e → UFPIOE_USAGE_ERROR e)
                             (\ case UFPIOE_USAGE_ERROR e → 𝕵 e; _ → 𝕹)

_UFPIOE_FPATHIO_ERROR ∷ Prism' UsageFPathIOError FPathIOError
_UFPIOE_FPATHIO_ERROR = prism' (\ e → UFPIOE_FPATHIO_ERROR e)
                        (\ case UFPIOE_FPATHIO_ERROR e → 𝕵 e; _ → 𝕹)

--------------------

instance Exception UsageFPathIOError

--------------------

instance Show UsageFPathIOError where
  show (UFPIOE_USAGE_ERROR e)      = show e
  show (UFPIOE_FPATHIO_ERROR    e) = show e

--------------------

instance AsUsageError UsageFPathIOError where
  _UsageError = _UFPIOE_USAGE_ERROR

--------------------

instance AsIOError UsageFPathIOError where
  _IOError = _UFPIOE_FPATHIO_ERROR ∘ _IOError

--------------------

instance AsFPathError UsageFPathIOError where
  _FPathError = _UFPIOE_FPATHIO_ERROR ∘ _FPathError

--------------------

instance Printable UsageFPathIOError where
  print (UFPIOE_USAGE_ERROR   e) = print e
  print (UFPIOE_FPATHIO_ERROR e) = print e

--------------------

instance HasCallstack UsageFPathIOError where
  callstack =
    let
      getter (UFPIOE_USAGE_ERROR   e) = e ⊣ callstack
      getter (UFPIOE_FPATHIO_ERROR e) = e ⊣ callstack
      setter (UFPIOE_USAGE_ERROR   e) cs =
        UFPIOE_USAGE_ERROR (e & callstack ⊢ cs)
      setter (UFPIOE_FPATHIO_ERROR e) cs =
        UFPIOE_FPATHIO_ERROR (e & callstack ⊢ cs)
    in
      lens getter setter

------------------------------------------------------------

{-| combined @UsageError@, @FPathError@, @IOError@, @CreateProcError@,
    @ProcExitError@ -}
data UsageFPProcIOError = UFPPIOE_UFPIO_ERROR UsageFPathIOError
                        | UFPPIOE_CPROC_ERROR CreateProcError
                        | UFPPIOE_PEXIT_ERROR ProcExitError
  deriving (Eq, Generic, NFData)

_UFPPIOE_UFPIO_ERROR ∷ Prism' UsageFPProcIOError UsageFPathIOError
_UFPPIOE_UFPIO_ERROR =
  prism' (\ e → UFPPIOE_UFPIO_ERROR e)
         (\ case UFPPIOE_UFPIO_ERROR e → 𝕵 e; _ → 𝕹)

_UFPPIOE_CPROC_ERROR ∷ Prism' UsageFPProcIOError CreateProcError
_UFPPIOE_CPROC_ERROR =
  prism' (\ e → UFPPIOE_CPROC_ERROR e)
         (\ case UFPPIOE_CPROC_ERROR e → 𝕵 e; _ → 𝕹)

_UFPPIOE_PEXIT_ERROR ∷ Prism' UsageFPProcIOError ProcExitError
_UFPPIOE_PEXIT_ERROR =
  prism' (\ e → UFPPIOE_PEXIT_ERROR e)
         (\ case UFPPIOE_PEXIT_ERROR e → 𝕵 e; _ → 𝕹)

--------------------

instance Exception UsageFPProcIOError

--------------------

instance Show UsageFPProcIOError where
  show (UFPPIOE_UFPIO_ERROR e) = show e
  show (UFPPIOE_CPROC_ERROR e) = show e
  show (UFPPIOE_PEXIT_ERROR e) = show e

--------------------

instance AsUsageError UsageFPProcIOError where
  _UsageError = _UFPPIOE_UFPIO_ERROR ∘ _UsageError

--------------------

instance AsIOError UsageFPProcIOError where
  _IOError = _UFPPIOE_CPROC_ERROR ∘ _IOError

--------------------

instance AsFPathError UsageFPProcIOError where
  _FPathError = _UFPPIOE_UFPIO_ERROR ∘ _FPathError

--------------------

instance AsCreateProcError UsageFPProcIOError where
  _CreateProcError = _UFPPIOE_CPROC_ERROR

--------------------

instance AsProcExitError UsageFPProcIOError where
  _ProcExitError =  _UFPPIOE_PEXIT_ERROR

--------------------

instance Printable UsageFPProcIOError where
  print (UFPPIOE_UFPIO_ERROR e) = print e
  print (UFPPIOE_CPROC_ERROR e) = print e
  print (UFPPIOE_PEXIT_ERROR e) = print e

--------------------

instance HasCallstack UsageFPProcIOError where
  callstack =
    let
      getter (UFPPIOE_UFPIO_ERROR e) = e ⊣ callstack
      getter (UFPPIOE_CPROC_ERROR e) = e ⊣ callstack
      getter (UFPPIOE_PEXIT_ERROR e) = e ⊣ callstack
      setter (UFPPIOE_UFPIO_ERROR   e) cs =
        UFPPIOE_UFPIO_ERROR (e & callstack ⊢ cs)
      setter (UFPPIOE_CPROC_ERROR e) cs =
        UFPPIOE_CPROC_ERROR (e & callstack ⊢ cs)
      setter (UFPPIOE_PEXIT_ERROR e) cs =
        UFPPIOE_PEXIT_ERROR (e & callstack ⊢ cs)
    in
      lens getter setter

------------------------------------------------------------

{-| combined @UsageError@, @FPathError@, @IOError@, @CreateProcError@,
    @ProcExitError@, @ParseError@ -}
data UsageParseFPProcIOError = UPFPPIOE_USAGE_ETC_ERROR UsageFPProcIOError
                             | UPFPPIOE_PARSE_ERROR ParseError
  deriving (Eq, Generic, NFData, Show)

_UPFPPIOE_USAGE_ETC_ERROR ∷ Prism' UsageParseFPProcIOError
                                   UsageFPProcIOError
_UPFPPIOE_USAGE_ETC_ERROR =
  prism' UPFPPIOE_USAGE_ETC_ERROR
         (\ case UPFPPIOE_USAGE_ETC_ERROR e → 𝕵 e; _ → 𝕹)

_UPFPPIOE_PARSE_ERROR ∷ Prism' UsageParseFPProcIOError ParseError
_UPFPPIOE_PARSE_ERROR =
  prism' UPFPPIOE_PARSE_ERROR (\ case UPFPPIOE_PARSE_ERROR e → 𝕵 e; _ → 𝕹)

instance Exception UsageParseFPProcIOError

instance Printable UsageParseFPProcIOError where
  print (UPFPPIOE_USAGE_ETC_ERROR e) = print e
  print (UPFPPIOE_PARSE_ERROR     e) = print e

instance HasCallstack UsageParseFPProcIOError where
  callstack =
    let
      getter (UPFPPIOE_USAGE_ETC_ERROR   e) = e ⊣ callstack
      getter (UPFPPIOE_PARSE_ERROR e)       = e ⊣ callstack
      setter (UPFPPIOE_USAGE_ETC_ERROR   e) cs =
        UPFPPIOE_USAGE_ETC_ERROR (e & callstack ⊢ cs)
      setter (UPFPPIOE_PARSE_ERROR e) cs =
        UPFPPIOE_PARSE_ERROR (e & callstack ⊢ cs)
    in
      lens getter setter

instance AsCreateProcError UsageParseFPProcIOError where
  _CreateProcError  = _UPFPPIOE_USAGE_ETC_ERROR ∘ _CreateProcError

instance AsFPathError UsageParseFPProcIOError where
  _FPathError  = _UPFPPIOE_USAGE_ETC_ERROR ∘ _FPathError

instance AsIOError UsageParseFPProcIOError where
  _IOError = _UPFPPIOE_USAGE_ETC_ERROR ∘ _IOError

instance AsParseError UsageParseFPProcIOError where
  _ParseError = _UPFPPIOE_PARSE_ERROR

instance AsProcExitError UsageParseFPProcIOError where
  _ProcExitError  = _UPFPPIOE_USAGE_ETC_ERROR ∘ _ProcExitError

instance AsUsageError UsageParseFPProcIOError where
  _UsageError  = _UPFPPIOE_USAGE_ETC_ERROR ∘ _UsageError

------------------------------------------------------------

{-| combined @UsageError@, @FPathError@, @IOError@, @TextualParseError@ -}
data UsageFPIOTPError = UFPIOTPE_USAGE_FPATH_IO_ERROR UsageFPathIOError
                      | UFPIOTPE_TPARSE_ERROR TextualParseError
  deriving (Eq, Generic, NFData)

_UFPIOTPE_USAGE_FPATH_IO_ERROR ∷ Prism' UsageFPIOTPError UsageFPathIOError
_UFPIOTPE_USAGE_FPATH_IO_ERROR = prism' (\ e → UFPIOTPE_USAGE_FPATH_IO_ERROR e)
                        (\ case UFPIOTPE_USAGE_FPATH_IO_ERROR e → 𝕵 e; _ → 𝕹)

_UFPIOTPE_TPARSE_ERROR ∷ Prism' UsageFPIOTPError TextualParseError
_UFPIOTPE_TPARSE_ERROR = prism' (\ e → UFPIOTPE_TPARSE_ERROR e)
                             (\ case UFPIOTPE_TPARSE_ERROR e → 𝕵 e; _ → 𝕹)

--------------------

instance Exception UsageFPIOTPError

--------------------

instance Show UsageFPIOTPError where
  show (UFPIOTPE_TPARSE_ERROR e)            = show e
  show (UFPIOTPE_USAGE_FPATH_IO_ERROR    e) = show e

--------------------

instance AsUsageError UsageFPIOTPError where
  _UsageError = _UFPIOTPE_USAGE_FPATH_IO_ERROR ∘ _UsageError

--------------------

instance AsTextualParseError UsageFPIOTPError where
  _TextualParseError = _UFPIOTPE_TPARSE_ERROR

--------------------

instance AsFPathError UsageFPIOTPError where
  _FPathError = _UFPIOTPE_USAGE_FPATH_IO_ERROR ∘ _FPathError

--------------------

instance AsIOError UsageFPIOTPError where
  _IOError = _UFPIOTPE_USAGE_FPATH_IO_ERROR ∘ _IOError

--------------------

instance Printable UsageFPIOTPError where
  print (UFPIOTPE_TPARSE_ERROR   e)       = print e
  print (UFPIOTPE_USAGE_FPATH_IO_ERROR e) = print e

--------------------

instance HasCallstack UsageFPIOTPError where
  callstack =
    let
      getter (UFPIOTPE_TPARSE_ERROR   e)       = e ⊣ callstack
      getter (UFPIOTPE_USAGE_FPATH_IO_ERROR e) = e ⊣ callstack
      setter (UFPIOTPE_TPARSE_ERROR   e) cs =
        UFPIOTPE_TPARSE_ERROR (e & callstack ⊢ cs)
      setter (UFPIOTPE_USAGE_FPATH_IO_ERROR e) cs =
        UFPIOTPE_USAGE_FPATH_IO_ERROR (e & callstack ⊢ cs)
    in
      lens getter setter

------------------------------------------------------------

{-| combined @UsageError@, @FPathError@, @IOError@, @TextualParseError@ -}
data UsageParseAesonFPPIOError = UPAFPPIOE_USAGE_FP_PROC_IO_ERROR UsageParseFPProcIOError
                               | UPAFPPIOE_AESON_ERROR AesonError
                               | UPAFPPIOE_TPARSE_ERROR TextualParseError
  deriving (Eq, Generic, NFData)

_UPAFPPIOE_USAGE_FP_PROC_IO_ERROR ∷ Prism' UsageParseAesonFPPIOError
                                           UsageParseFPProcIOError
_UPAFPPIOE_USAGE_FP_PROC_IO_ERROR =
  prism' (\ e → UPAFPPIOE_USAGE_FP_PROC_IO_ERROR e)
         (\ case UPAFPPIOE_USAGE_FP_PROC_IO_ERROR e → 𝕵 e; _ → 𝕹)

_UPAFPPIOE_AESON_ERROR ∷ Prism' UsageParseAesonFPPIOError AesonError
_UPAFPPIOE_AESON_ERROR = prism' (\ e → UPAFPPIOE_AESON_ERROR e)
                                (\ case UPAFPPIOE_AESON_ERROR e → 𝕵 e; _ → 𝕹)

_UPAFPPIOE_TPARSE_ERROR ∷ Prism' UsageParseAesonFPPIOError TextualParseError
_UPAFPPIOE_TPARSE_ERROR = prism' (\ e → UPAFPPIOE_TPARSE_ERROR e)
                                 (\ case UPAFPPIOE_TPARSE_ERROR e → 𝕵 e; _ → 𝕹)

--------------------

instance Exception UsageParseAesonFPPIOError

--------------------

instance Show UsageParseAesonFPPIOError where
  show (UPAFPPIOE_AESON_ERROR            e) = show e
  show (UPAFPPIOE_TPARSE_ERROR           e) = show e
  show (UPAFPPIOE_USAGE_FP_PROC_IO_ERROR e) = show e

--------------------

instance AsUsageError UsageParseAesonFPPIOError where
  _UsageError = _UPAFPPIOE_USAGE_FP_PROC_IO_ERROR ∘ _UsageError

--------------------

instance AsTextualParseError UsageParseAesonFPPIOError where
  _TextualParseError = _UPAFPPIOE_TPARSE_ERROR

--------------------

instance AsAesonError UsageParseAesonFPPIOError where
  _AesonError = _UPAFPPIOE_AESON_ERROR

--------------------

instance AsFPathError UsageParseAesonFPPIOError where
  _FPathError = _UPAFPPIOE_USAGE_FP_PROC_IO_ERROR ∘ _FPathError

--------------------

instance AsIOError UsageParseAesonFPPIOError where
  _IOError = _UPAFPPIOE_USAGE_FP_PROC_IO_ERROR ∘ _IOError

--------------------

instance AsCreateProcError UsageParseAesonFPPIOError where
  _CreateProcError = _UPAFPPIOE_USAGE_FP_PROC_IO_ERROR ∘ _CreateProcError

--------------------

instance AsProcExitError UsageParseAesonFPPIOError where
  _ProcExitError = _UPAFPPIOE_USAGE_FP_PROC_IO_ERROR ∘ _ProcExitError

--------------------

instance Printable UsageParseAesonFPPIOError where
  print (UPAFPPIOE_AESON_ERROR   e)          = print e
  print (UPAFPPIOE_TPARSE_ERROR  e)          = print e
  print (UPAFPPIOE_USAGE_FP_PROC_IO_ERROR e) = print e

--------------------

instance HasCallstack UsageParseAesonFPPIOError where
  callstack =
    let
      getter (UPAFPPIOE_AESON_ERROR   e)          = e ⊣ callstack
      getter (UPAFPPIOE_TPARSE_ERROR  e)          = e ⊣ callstack
      getter (UPAFPPIOE_USAGE_FP_PROC_IO_ERROR e) = e ⊣ callstack
      setter (UPAFPPIOE_AESON_ERROR   e) cs =
        UPAFPPIOE_AESON_ERROR (e & callstack ⊢ cs)
      setter (UPAFPPIOE_TPARSE_ERROR  e) cs =
        UPAFPPIOE_TPARSE_ERROR (e & callstack ⊢ cs)
      setter (UPAFPPIOE_USAGE_FP_PROC_IO_ERROR e) cs =
        UPAFPPIOE_USAGE_FP_PROC_IO_ERROR (e & callstack ⊢ cs)
    in
      lens getter setter

-- that's all, folks! ----------------------------------------------------------
