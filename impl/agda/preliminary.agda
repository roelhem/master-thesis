{-# OPTIONS --type-in-type #-}

module preliminary where

open import Prelude.Unit
open import Prelude.Empty
open import Prelude.Product
open import Prelude.Sum renaming (Either to _+_)
open import Prelude.Equality

record Category (obj : Set) : Set where
  field
    Hom : obj → obj → Set
    id  : {a : obj} → Hom a a
    _∘_ : {a b c : obj} → Hom b c → Hom a b → Hom a c
open Category {{...}} public

instance
  set-category : Category Set
  set-category = record { Hom = λ a b → (a → b) 
                        ; id  = λ a → a
                        ; _∘_ = λ f g → ( λ x → f ( g x ) )
                        }