module StdMain.UsageError
  ( AsUsageError( _UsageError ), UsageError, UsageIOError
  , readUsage, throwUsage, usageError )
where

-- base --------------------------------

import Control.Exception  ( Exception )
import Control.Monad      ( return )
import Data.Eq            ( Eq( (==) ) )
import Data.Function      ( ($), (&), id )
import Data.Maybe         ( Maybe( Just, Nothing ), maybe )
import GHC.Stack          ( CallStack, HasCallStack, callStack )
import Text.Read          ( Read, readMaybe )
import Text.Show          ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), toString, toText )

-- fpath -------------------------------

import FPath.Error.FPathError  ( AsFPathError( _FPathError ), FPathIOError )

-- has-callstack -----------------------

import HasCallstack  ( HasCallstack( callstack ) )

-- lens --------------------------------

import Control.Lens.Lens    ( lens )
import Control.Lens.Prism   ( Prism', prism' )
import Control.Lens.Review  ( (#) )

-- monaderror-io -----------------------

import MonadError.IO.Error  ( AsIOError( _IOError ), IOError )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens  ( (⊣), (⊢) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, throwError )

-- text --------------------------------

import Data.Text  ( Text )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmtT )

--------------------------------------------------------------------------------

data UsageError = UsageError { _txt ∷ Text, _callstack ∷ CallStack }
  deriving Show

----------------------------------------

instance Exception UsageError

----------------------------------------

instance Eq UsageError where
  (UsageError a _) == (UsageError b _) = a == b

----------------------------------------

instance HasCallstack UsageError where
  callstack = lens _callstack (\ eu cs → eu { _callstack = cs })

----------------------------------------

class AsUsageError ε where
  _UsageError ∷ Prism' ε UsageError

--------------------

instance AsUsageError UsageError where
  _UsageError = id

--------------------

instance Printable UsageError where
  print = P.text ∘ _txt

------------------------------------------------------------

usageError ∷ ∀ τ ε . (AsUsageError ε, Printable τ, HasCallStack) ⇒ τ → ε
usageError t = _UsageError # UsageError (toText t) callStack

----------------------------------------

throwUsage ∷ ∀ τ ε ω η . (Printable τ, AsUsageError ε, MonadError ε η) ⇒ τ → η ω
throwUsage t = throwError $ usageError t

----------------------------------------

readUsage ∷ ∀ τ ε ω η . (AsUsageError ε, MonadError ε η, Read ω, Printable τ) ⇒
            τ → η ω
readUsage s = let errMsg = [fmtT|failed to parse: '%T'|] s
               in maybe (throwUsage $ errMsg) return (readMaybe $ toString s)

------------------------------------------------------------

data UsageIOError = UIOE_USAGE_ERROR UsageError
                  | UIOE_IO_ERROR    IOError

_UIOE_USAGE_ERROR ∷ Prism' UsageIOError UsageError
_UIOE_USAGE_ERROR = prism' (\ e → UIOE_USAGE_ERROR e)
                           (\ case UIOE_USAGE_ERROR e → Just e; _ → Nothing)

_UIOE_IO_ERROR ∷ Prism' UsageIOError IOError
_UIOE_IO_ERROR = prism' (\ e → UIOE_IO_ERROR e)
                        (\ case UIOE_IO_ERROR e → Just e; _ → Nothing)

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

data UsageFPathIOError = UFPIOE_USAGE_ERROR   UsageError
                       | UFPIOE_FPATHIO_ERROR FPathIOError

_UFPIOE_USAGE_ERROR ∷ Prism' UsageFPathIOError UsageError
_UFPIOE_USAGE_ERROR = prism' (\ e → UFPIOE_USAGE_ERROR e)
                           (\ case UFPIOE_USAGE_ERROR e → Just e; _ → Nothing)

_UFPIOE_FPATHIO_ERROR ∷ Prism' UsageFPathIOError FPathIOError
_UFPIOE_FPATHIO_ERROR = prism' (\ e → UFPIOE_FPATHIO_ERROR e)
                        (\ case UFPIOE_FPATHIO_ERROR e → Just e; _ → Nothing)

--------------------

instance Exception UsageFPathIOError

--------------------

instance Show UsageFPathIOError where
  show (UFPIOE_USAGE_ERROR e) = show e
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
  print (UFPIOE_USAGE_ERROR e) = print e
  print (UFPIOE_FPATHIO_ERROR    e) = print e

--------------------

instance HasCallstack UsageFPathIOError where
  callstack =
    let
      getter (UFPIOE_USAGE_ERROR e) = e ⊣ callstack
      getter (UFPIOE_FPATHIO_ERROR    e) = e ⊣ callstack
      setter (UFPIOE_USAGE_ERROR e) cs = UFPIOE_USAGE_ERROR (e & callstack ⊢ cs)
      setter (UFPIOE_FPATHIO_ERROR    e) cs = UFPIOE_FPATHIO_ERROR    (e & callstack ⊢ cs)
    in
      lens getter setter

-- that's all, folks! ----------------------------------------------------------
