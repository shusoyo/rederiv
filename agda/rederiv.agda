module rederiv where

open import Agda.Builtin.Char
open import Agda.Builtin.Bool

-- Regular expressions
data regexp : Set where
  ∅    : regexp
  ε    : regexp
  char : Char → regexp
  _·_  : regexp → regexp → regexp
  star : regexp → regexp
  _+_  : regexp → regexp → regexp
  _&_  : regexp → regexp → regexp
  ¬_   : regexp → regexp

infixl 5 _·_
infixl 4 _+_ _&_
infixl 3 ¬_

_ : regexp
_ = ε · (char 'a') · (star (char 'b'))  

-- dfa for regular expressions
record DFA : Set₁ where
  constructor dfa
  field
    State      : Set
    startState : State
    isAccept   : State → Bool
    transition : State → Char → State
open DFA
