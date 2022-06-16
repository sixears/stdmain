{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}

module StdMain.ProcOutputParseError
  ( AsProcOutputParseError(..), AsTextError(..)
  , ProcOPError, ProcOutputParseError, ScriptError, TextError
  , UsageParseFPProcIOOPError
  , asProcOutputParseError, asTextError, eCatchProcOPE
  , throwAsProcOutputParseError, throwAsTextError, throwToTextError, ҩ, ҩҩ
  )
where

import Base1T

-- base --------------------------------

import GHC.Generics  ( Generic )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- fpath -------------------------------

import FPath.Error.FPathError  ( AsFPathError( _FPathError ) )

-- monaderror-io -----------------------

import MonadError  ( eitherME )

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

throwAsProcOutputParseError ∷ ∀ ε ρ α η .
                              (AsProcOutputParseError ε, MonadError ε η,
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

----------------------------------------

{-| Catch an `Either`, e.g., a MonadThrow or similar, and turn it into a
    `MonadError` of `ProcOutputParseError`.  -}
eCatchProcOPE ∷ ∀ ε ε' α η .
                (MonadError ε η,HasCallStack,AsProcOutputParseError ε,Show ε') ⇒
                𝔼 ε' α → η α
eCatchProcOPE = eitherME (asProcOutputParseError ∘ show)

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

------------------------------------------------------------

{- | General-purpose text error -}
newtype TextError = TextError (𝕋, CallStack)
  deriving Generic
  deriving anyclass NFData

instance Exception TextError

instance Show TextError where
  show (TextError (t,_)) = show t

instance Eq TextError where
  TextError (t,_) == TextError (t',_) = t == t'

instance HasCallstack TextError where
  callstack = lens (\ (TextError (_,cs)) → cs)
                   (\ (TextError (t,_)) cs →
                       TextError (t,cs))

instance Printable TextError where
  print (TextError (t,_)) = P.text $ "TextError: " ⊕ t

------------------------------------------------------------

{- | An error that might be of type `ProcOutputParseError` -}
class AsTextError ε where
  _TextError ∷ Prism' ε TextError

instance AsTextError TextError where
  _TextError = id

----------------------------------------

asTextError ∷ (AsTextError ε, HasCallStack, Printable ρ) ⇒ ρ → ε
asTextError t = _TextError # TextError (toText t,callStack)

--------------------

throwAsTextError  ∷ ∀ ε α η . (AsTextError ε, MonadError ε η, HasCallStack)⇒
                    𝕋 → η α
throwAsTextError t = throwError $ _TextError # TextError (t,callStack)

----------

ҩ :: ∀ ε α η . (AsTextError ε, MonadError ε η, HasCallStack) ⇒ 𝕋 -> η α
ҩ = throwAsTextError

----------------------------------------

{-| Convert some printable error to a `TextError`, probably for throwing into
    a `ScriptError`. -}
throwToTextError ∷ ∀ ε β α η . (AsTextError ε, Printable α, MonadError ε η) ⇒
              𝔼 α β → η β
throwToTextError x = case x of
                       𝕷 e → throwAsTextError (toText e)
                       𝕽 r → return r

----------

ҩҩ ∷ ∀ ε β α η . (AsTextError ε, Printable α, MonadError ε η) ⇒ 𝔼 α β → η β
ҩҩ = throwToTextError

------------------------------------------------------------

{-| Generic error for scripts, intended to encompass all other general-purpose
    script error types (so may be upgraded in future to include more things.
 -}
data ScriptError = UPFPPIOOP_ERROR UsageParseFPProcIOOPError
                 | B_ERROR         TextError
  deriving (Eq,Generic,Show)

_UPFPPIOOP_ERROR ∷ Prism' ScriptError UsageParseFPProcIOOPError
_UPFPPIOOP_ERROR =
  prism' (\ e → UPFPPIOOP_ERROR e) (\ case UPFPPIOOP_ERROR e → 𝕵 e; _ → 𝕹)

_B_ERROR ∷ Prism' ScriptError TextError
_B_ERROR = prism' (\ e → B_ERROR e) (\ case B_ERROR e → 𝕵 e; _ →𝕹)

instance Exception ScriptError

instance HasCallstack ScriptError where
  callstack =
    lens (\ case (UPFPPIOOP_ERROR popepe) → popepe  ⊣ callstack
                 (B_ERROR popeope)        → popeope ⊣ callstack)
         (\ pe cs → case pe of
                      (UPFPPIOOP_ERROR popepe) →
                        UPFPPIOOP_ERROR $ popepe & callstack ⊢ cs
                      (B_ERROR popeope) → B_ERROR $ popeope & callstack ⊢ cs
         )

instance AsCreateProcError ScriptError where
 _CreateProcError = _UPFPPIOOP_ERROR ∘ _CreateProcError

instance AsIOError ScriptError where
  _IOError = _UPFPPIOOP_ERROR ∘ _IOError

instance AsFPathError ScriptError where
  _FPathError = _UPFPPIOOP_ERROR ∘ _FPathError

instance AsProcExitError ScriptError where
  _ProcExitError = _UPFPPIOOP_ERROR ∘ _ProcExitError

instance Printable ScriptError where
  print (UPFPPIOOP_ERROR pexe   ) = print pexe
  print (B_ERROR         popeope)  = print popeope

instance AsProcOutputParseError ScriptError where
  _ProcOutputParseError = _UPFPPIOOP_ERROR ∘ _ProcOutputParseError

instance AsUsageError ScriptError where
  _UsageError  = _UPFPPIOOP_ERROR ∘ _UsageError

instance AsTextError ScriptError where
  _TextError = _B_ERROR

-- that's all, folks! ----------------------------------------------------------
