{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}

module StdMain.ProcOutputParseError
  ( AsProcOutputParseError(..), ProcOPError, ProcOutputParseError
  , asProcOutputParseError, throwAsProcOutputParseError )
where

import Base1T

-- base --------------------------------

import GHC.Generics  ( Generic )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- fpath -------------------------------

import FPath.Error.FPathError  ( AsFPathError( _FPathError ) )

-- monadio-plus ------------------------

import MonadIO.Error.CreateProcError  ( AsCreateProcError( _CreateProcError )
                                      , ProcError )
import MonadIO.Error.ProcExitError    ( AsProcExitError( _ProcExitError ) )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import StdMain.UsageError  ( AsUsageError( _UsageError ),
                             UsageParseFPProcIOError )

--------------------------------------------------------------------------------

{- | Error whilst parsing process output (e.g., output from df, or…) -}
newtype ProcOutputParseError = ProcOutputParseError (𝕋, CallStack)
  deriving Generic
  deriving anyclass NFData

instance Exception ProcOutputParseError

instance Show ProcOutputParseError where
  show (ProcOutputParseError (t,_)) = show t

instance Eq ProcOutputParseError where
  ProcOutputParseError (t,_) == ProcOutputParseError (t',_) = t == t'

instance HasCallstack ProcOutputParseError where
  callstack = lens (\ (ProcOutputParseError (_,cs)) → cs)
                   (\ (ProcOutputParseError (t,_)) cs →
                       ProcOutputParseError (t,cs))

instance Printable ProcOutputParseError where
  print (ProcOutputParseError (t,_)) = P.text $ "ProcOutputParseError: " ⊕ t

------------------------------------------------------------

{- | An error that might be of type `ProcOutputParseError` -}
class AsProcOutputParseError ε where
  _ProcOutputParseError ∷ Prism' ε ProcOutputParseError

instance AsProcOutputParseError ProcOutputParseError where
  _ProcOutputParseError = id

asProcOutputParseError ∷ (AsProcOutputParseError ε, HasCallStack, Printable ρ) ⇒
                         ρ → ε
asProcOutputParseError t =
  _ProcOutputParseError # ProcOutputParseError (toText t,callStack)

throwAsProcOutputParseError ∷ (AsProcOutputParseError ε, MonadError ε η,
                               HasCallStack, Printable ρ) ⇒
                              ρ → η α
throwAsProcOutputParseError t =
  throwError $ _ProcOutputParseError # ProcOutputParseError (toText t,callStack)

------------------------------------------------------------

{- | A concrete process error, possibly including `ProcOutputParseError` -}
data ProcOPError = POPE_PROC_ERROR         ProcError
                 | POPE_OUTPUT_PARSE_ERROR ProcOutputParseError
  deriving (Eq,Generic,NFData,Show)

_POPE_PROC_ERROR ∷ Prism' ProcOPError ProcError
_POPE_PROC_ERROR = prism' (\ e → POPE_PROC_ERROR e)
                          (\ case POPE_PROC_ERROR e → 𝕵 e; _ → 𝕹)

_POPE_OUTPUT_PARSE_ERROR ∷ Prism' ProcOPError ProcOutputParseError
_POPE_OUTPUT_PARSE_ERROR = prism' (\ e → POPE_OUTPUT_PARSE_ERROR e)
                                  (\ case POPE_OUTPUT_PARSE_ERROR e → 𝕵 e; _ →𝕹)

instance Exception ProcOPError

instance HasCallstack ProcOPError where
  callstack =
    lens (\ case (POPE_PROC_ERROR   popepe) → popepe ⊣ callstack
                 (POPE_OUTPUT_PARSE_ERROR popeope)   → popeope   ⊣ callstack)
         (\ pe cs → case pe of
                      (POPE_PROC_ERROR popepe) →
                        POPE_PROC_ERROR $ popepe & callstack ⊢ cs
                      (POPE_OUTPUT_PARSE_ERROR popeope) →
                        POPE_OUTPUT_PARSE_ERROR $ popeope & callstack ⊢ cs
         )

instance AsCreateProcError ProcOPError where
 _CreateProcError = _POPE_PROC_ERROR ∘ _CreateProcError

instance AsIOError ProcOPError where
  _IOError = _POPE_PROC_ERROR ∘ _IOError

instance AsFPathError ProcOPError where
  _FPathError = _POPE_PROC_ERROR ∘ _FPathError

instance AsProcExitError ProcOPError where
  _ProcExitError = _POPE_PROC_ERROR ∘ _ProcExitError

instance Printable ProcOPError where
  print (POPE_PROC_ERROR       pexe) = print pexe
  print (POPE_OUTPUT_PARSE_ERROR popeope)  = print popeope

instance AsProcOutputParseError ProcOPError where
  _ProcOutputParseError = _POPE_OUTPUT_PARSE_ERROR

------------------------------------------------------------

{- | A concrete main error, possibly including `ProcOutputParseError` -}
data UsageParseFPProcIOOPError = UPFPIOP_USAGE_ETC_ERROR UsageParseFPProcIOError
                               | UPFPIOP_OUTPUT_ERROR    ProcOutputParseError
  deriving (Eq,Generic,Show)

_UPFPIOP_USAGE_ETC_ERROR ∷ Prism' UsageParseFPProcIOOPError
                                  UsageParseFPProcIOError
_UPFPIOP_USAGE_ETC_ERROR =
  prism' (\ e → UPFPIOP_USAGE_ETC_ERROR e)
         (\ case UPFPIOP_USAGE_ETC_ERROR e → 𝕵 e; _ → 𝕹)

_UPFPIOP_OUTPUT_ERROR ∷ Prism' UsageParseFPProcIOOPError ProcOutputParseError
_UPFPIOP_OUTPUT_ERROR = prism' (\ e → UPFPIOP_OUTPUT_ERROR e)
                                  (\ case UPFPIOP_OUTPUT_ERROR e → 𝕵 e; _ →𝕹)

instance Exception UsageParseFPProcIOOPError

instance HasCallstack UsageParseFPProcIOOPError where
  callstack =
    lens (\ case (UPFPIOP_USAGE_ETC_ERROR   popepe) → popepe ⊣ callstack
                 (UPFPIOP_OUTPUT_ERROR popeope)   → popeope   ⊣ callstack)
         (\ pe cs → case pe of
                      (UPFPIOP_USAGE_ETC_ERROR popepe) →
                        UPFPIOP_USAGE_ETC_ERROR $ popepe & callstack ⊢ cs
                      (UPFPIOP_OUTPUT_ERROR popeope) →
                        UPFPIOP_OUTPUT_ERROR $ popeope & callstack ⊢ cs
         )

instance AsCreateProcError UsageParseFPProcIOOPError where
 _CreateProcError = _UPFPIOP_USAGE_ETC_ERROR ∘ _CreateProcError

instance AsIOError UsageParseFPProcIOOPError where
  _IOError = _UPFPIOP_USAGE_ETC_ERROR ∘ _IOError

instance AsFPathError UsageParseFPProcIOOPError where
  _FPathError = _UPFPIOP_USAGE_ETC_ERROR ∘ _FPathError

instance AsProcExitError UsageParseFPProcIOOPError where
  _ProcExitError = _UPFPIOP_USAGE_ETC_ERROR ∘ _ProcExitError

instance Printable UsageParseFPProcIOOPError where
  print (UPFPIOP_USAGE_ETC_ERROR       pexe) = print pexe
  print (UPFPIOP_OUTPUT_ERROR popeope)  = print popeope

instance AsProcOutputParseError UsageParseFPProcIOOPError where
  _ProcOutputParseError = _UPFPIOP_OUTPUT_ERROR ∘ _ProcOutputParseError

instance AsUsageError UsageParseFPProcIOOPError where
  _UsageError  = _UPFPIOP_USAGE_ETC_ERROR ∘ _UsageError

-- that's all, folks! ----------------------------------------------------------
