{-# LANGUAGE UnicodeSyntax #-}
module StdMain.StdOptions
  ( DryRunLevel
  , HasDryRun
  , HasDryRunLevel(dryRunLevel, level)
  , ReadDryRunLevel
  , StdOptions
  , askDryRunL
  , callstackOnError
  , dryRun1P
  , dryRun2P
  , dryRunLvl
  , dryRunNum
  , dryRunOff
  , dryRunOn
  , dryRunP
  , ifDryRun
  , ifDryRunEq
  , ifDryRunGE
  , options
  , parseStdOptions
  , profCallstackOnError
  , unlessDryRunGE
  ) where

import Base1T
import Prelude ( pred, succ )

-- exited ------------------------------

import Exited ( CallstackOnError(..), ProfCallstackOnError(..) )

-- lens --------------------------------

import Control.Lens.Getter ( view )
import Control.Lens.Iso    ( iso )

-- log-plus ----------------------------

import Log.HasSeverity ( HasSeverity(severity) )

-- logging-effect ----------------------

import Control.Monad.Log ( Severity(Debug, Warning) )

-- mtl ---------------------------------

import Control.Monad.Reader ( MonadReader, ask )

-- natural-plus ------------------------

import Natural ( AtMost(Cons, Nil), Countable(count), Nat(S), Natty, One, Two,
                 atMost, atMostOne, atMostTwo, count, four, replicate, three )

-- optparse-applicative ----------------

import Options.Applicative ( FlagFields, Mod, Parser, flag, flag', internal,
                             long, short )

-- optparse-plus -------------------------

import OptParsePlus ( parsecOption )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import StdMain.VerboseOptions ( HasVerboseOptions(verboseOptions),
                                VerboseOptions, defVOpts )

--------------------------------------------------------------------------------

{- | Curryable if-then-else; flippety-`Data.Function.flip` of `Data.Bool.bool`.
 -}
ifThenElse ∷ Bool → a → a → a
ifThenElse b t e = if b then t else e

----------------------------------------

data DryRunN = DryRunN deriving (Show)

newtype DryRunLevel n = DryRunLevel { _level :: AtMost n DryRunN }

class HasDryRunLevel n c | c → n where
  dryRunLevel ∷ Lens' c (DryRunLevel n)
  level       ∷ Lens' c (AtMost n DryRunN)
  {-# INLINE level #-}
  level       =  dryRunLevel ∘ level
  dryRunNum   ∷ c → ℕ
  dryRunNum   = count ∘ view dryRunLevel

instance HasDryRunLevel n (DryRunLevel n) where
  dryRunLevel = id
  level       = iso ( \ (DryRunLevel x) → x) DryRunLevel

instance HasDryRunLevel n (AtMost n DryRunN) where
  dryRunLevel = lens DryRunLevel (const ∘ view level)

instance Show (DryRunLevel n) where
  show (DryRunLevel d) = [fmt|DryRun: %d|] (count d)

type DryRun = DryRunLevel One
type HasDryRun = HasDryRunLevel One

instance Countable (DryRunLevel ν) where
  count (DryRunLevel n) = count n

------------------------------------------------------------

type ReadDryRunLevel ν η = MonadReader (DryRunLevel ν) η

askDryRunL ∷ ReadDryRunLevel ν η ⇒ η (DryRunLevel ν)
askDryRunL = ask

----------------------------------------

dryRunLvl' ∷ DryRunLevel n → ℕ
dryRunLvl' (DryRunLevel d) = count d

dryRunLvl ∷ HasDryRunLevel n s ⇒ s → ℕ
dryRunLvl = dryRunLvl' ∘ view dryRunLevel

ifDryRunP ∷ ReadDryRunLevel ν η ⇒ (ℕ → 𝔹) → α → α → η α
ifDryRunP f go nogo = (\ drl → ifThenElse (f (dryRunLvl drl)) go nogo) ⊳ ask

ifDryRunEq ∷ ReadDryRunLevel ν η ⇒ ℕ → α → α → η α
ifDryRunEq i = ifDryRunP (≡ i)

ifDryRunGE ∷ ReadDryRunLevel ν η ⇒ ℕ → α → α → η α
ifDryRunGE i = ifDryRunP (≥ i)

ifDryRun ∷ ReadDryRunLevel ν η ⇒ α → α → η α
ifDryRun = ifDryRunGE 1

unlessDryRunGE ∷ ReadDryRunLevel ν η ⇒ ℕ → α → α → η α
unlessDryRunGE i d n = ifDryRunGE i n d

----------------------------------------

flagDryRun ∷ Parser DryRunN
-- marked as 'internal' because better help is in the Standard options footer
flagDryRun = flag' DryRunN (long "dry-run" ⊕ internal)

dryRunOff ∷ DryRunLevel ('S n)
dryRunOff = DryRunLevel Nil

dryRunOn ∷ DryRunLevel ('S n)
dryRunOn  = DryRunLevel (Cons DryRunN Nil)

----------------------------------------

dryRunP ∷ Natty ν → Parser (DryRunLevel ν)
dryRunP n = DryRunLevel ⊳ atMost n flagDryRun

----------

dryRun1P ∷ Parser DryRun
dryRun1P = DryRunLevel ⊳ atMostOne flagDryRun

----------

dryRun2P ∷ Parser (DryRunLevel Two)
dryRun2P = DryRunLevel ⊳ atMostTwo flagDryRun

------------------------------------------------------------

{- | Default Severity level; start with Warning, -v goes to Notice then
     Informational. -}
defaultSev ∷ Severity
defaultSev = Warning

----------------------------------------

data StdOptions ν α = StdOptions { _nonBaseOptions       :: α
                                 , _verboseOptions       :: VerboseOptions
                                 , _dryRunLevel          :: DryRunLevel ν
                                 , _callstackOnError     :: CallstackOnError
                                 , _profCallstackOnError :: ProfCallstackOnError
                                 }
  deriving (Show)

instance HasDryRunLevel ν (StdOptions ν α) where
  dryRunLevel = lens _dryRunLevel (\ so drl → so { _dryRunLevel = drl })

instance HasVerboseOptions (StdOptions ν α) where
  verboseOptions = lens _verboseOptions (\ so vo → so { _verboseOptions = vo })

instance HasSeverity (StdOptions ν α) where
  severity = verboseOptions ∘ severity

callstackOnError ∷ Lens' (StdOptions ν α) CallstackOnError
callstackOnError = lens _callstackOnError (\ s c → s { _callstackOnError = c })

profCallstackOnError ∷ Lens' (StdOptions ν α) ProfCallstackOnError
profCallstackOnError =
  lens _profCallstackOnError (\ s c → s { _profCallstackOnError = c })

options ∷ Lens' (StdOptions ν α) α
options = lens _nonBaseOptions
               (\ s nonBaseOptions → s { _nonBaseOptions = nonBaseOptions })

{-| parser fon the stdoptions; namely -v, --quiet, --debug, --verbose,
    --callstack-on-error, --prof-callstack-on-error -}
parseStdOptions ∷ Natty ν → Parser α → Parser (StdOptions ν α)
parseStdOptions n p =
  let -- up to n flags, each invoking an instance of a function
      flagn ∷ Natty ν → Mod FlagFields () → (α → α) → α → Parser α
      flagn i m f a =
        let replicate' ∷ ℕ → β → [β]
            replicate' = replicate
        in  (\ c → foldr ($) a (replicate' (count c) f)) ⊳ atMost i (flag' () m)

      -- flagsev i m f creates a flag to manage severity; it may be invoked
      -- up to m times, it has attributes m, and each time it calls f to adjust
      -- the severity
      flagsev ∷ Natty ν → Mod FlagFields () → (Severity → Severity)
              → Parser VerboseOptions
      flagsev i m f = defVOpts ⊳ flagn i m f defaultSev
      -- marked as 'internal' because a better description is in the Standard
      -- Options footer
      flagv         = flagsev three (short 'v' ⊕ internal) succ
      -- "--quiet" flag
      flagq         = flagsev four (long "quiet" ⊕ internal)
                              pred
      -- "--debug" flag
      flagd         = defVOpts ⊳ flag' Debug (long "debug" ⊕ internal)
      verbose       = parsecOption (long "verbose")
   in StdOptions ⊳ p
                 ⊵ (flagv ∤ flagq ∤ flagd ∤ verbose)
                 ⊵ dryRunP n
                 ⊵ flag NoCallstackOnError CallstackOnError
                        (long "callstack-on-error" ⊕ short '!')
                 ⊵ flag NoProfCallstackOnError ProfCallstackOnError
                        (long "prof-callstack-on-error" ⊕ short '#')

-- that's all, folks! ----------------------------------------------------------
