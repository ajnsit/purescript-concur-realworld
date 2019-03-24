-- Simple form validation

-- | Where there are forms, there is inevitably validation. We often need to ensure that user
-- | input passes a few checks before allowing them to submit a form to the server. This module
-- | provides generic validation that can be used in all sorts of different forms, like validating
-- | an input string is long enough, that a username is well-formed, or that a required field is
-- | filled in.
module Conduit.Form.Validation where

import Prelude

import Conduit.Data.Avatar (Avatar)
import Conduit.Data.Avatar as Avatar
import Conduit.Data.Email (Email(..))
import Conduit.Data.Username (Username)
import Conduit.Data.Username as Username
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..))
import Data.String as String

data FormValidation e a b = FormValidation (a -> Either e b)

mkValidation :: forall e a b. (a -> Either e b) -> FormValidation e a b
mkValidation = FormValidation

runValidation :: forall e a b. FormValidation e a b -> a -> Either e b
runValidation (FormValidation f) a = f a

-- | This short list of errors represent the only ways in which validation could have failed
-- | on a given field. As our application grows, we might revise this type so that each form
-- | and field has its own small subset of possible errors using extensible sum types (variants).
-- | For now, this small type covers all potential validation errors.
data FormError
  = Required
  | TooShort
  | TooLong
  | InvalidEmail
  | InvalidUsername
  | InvalidAvatar

-- | When a field has failed to pass validation, it will produce an error instead of a success
-- | value. But we don't want to show our users something they'd never see in usual English like
-- | `TooLong`. Instead, we'll produce human-readable text for each possible error.
errorToString :: FormError -> String
errorToString = case _ of
  Required -> "This field is required."
  TooShort -> "Not enough characters entered"
  TooLong -> "Too many characters entered"
  InvalidEmail -> "Invalid email address"
  InvalidUsername -> "Invalid username"
  InvalidAvatar -> "Invalid image URL"

-- Field validators

-- | For the most part, the generic validation functions we'll write just need to transform some
-- | input into some output, possibly failing, without the need to refer to any other values in the
-- | form or perform effects.

-- | The first validator we'll write verifies that the input is not empty. The same validator can
-- | apply to any input value that is a monoid, as the `Monoid` type class represents 'emptiness'
-- | with the `mempty` value. We'll just check whether the input is the empty value.
required :: ∀ a. Eq a => Monoid a => FormValidation FormError a a
required = mkValidation $ cond (_ /= mempty) Required

-- | This validator ensures that an input string is longer than the provided lower limit.
minLength :: Int -> FormValidation FormError String String
minLength n = mkValidation $ cond (\str -> String.length str > n) TooShort

-- | This validator ensures that an input string is shorter than the provided upper limit.
maxLength :: Int -> FormValidation FormError String String
maxLength n = mkValidation $ cond (\str -> String.length str <= n) TooLong

-- | This validator ensures that an input string is a valid email address, using a fairly naive
-- | requirement that it at least includes the `@` symbol. This is our first example of a validator
-- | that returns a different output value than its input value.
emailFormat :: FormValidation FormError String Email
emailFormat = mkValidation $ map Email <<< cond (String.contains (String.Pattern "@")) InvalidEmail

-- | This validator ensures that an input string is a valid username. Usernames in Conduit use the
-- | smart constructor pattern, so we can't construct a username directly -- we'll need to defer
-- | to the `parse` helper function exported by `Conduit.Data.Username`. Since that function returns
-- | a `Maybe` value, we'll use the `note` helper from `Data.Either` to turn the `Nothing` case into
-- | an error.
usernameFormat :: FormValidation FormError String Username
usernameFormat = mkValidation $ note InvalidUsername <<< Username.parse

-- | Our avatar validator follows the same pattern, validating and transforming an input string into
-- | an `Avatar`.
avatarFormat :: FormValidation FormError String Avatar
avatarFormat = mkValidation $ note InvalidAvatar <<< Avatar.parse

-- Utilities

-- | Validation often relies on a true/false function (a predicate), where `true` should return the
-- | input value and `false` should return the correct error. This pattern happens often enough that
-- | I've created a small helper, `cond`, which abstracts the pattern.
cond :: forall a. (a -> Boolean) -> FormError -> a -> Either FormError a
cond f err a = if f a then pure a else Left err

-- | Sometimes we'd like to validate an input only if it isn't empty. This is useful for optional
-- | fields: if you've provided a value, we'll validate it, but if you haven't, then you should
-- | still be able to submit the form without error. For instance, we might allow a user to
-- | optionally provide an email address, but if they do, it must be valid.
-- |
-- | This helper function lets us transform a set of validation rules so that they only apply when
-- | the input is not empty. It isn't used in this module, but is used in the various forms.
toOptional :: ∀ a b
   . Monoid a
  => Eq a
  => FormValidation FormError a b
  -> FormValidation FormError a (Maybe b)
toOptional v = mkValidation \val ->
  if val == mempty
    then pure Nothing
    else Just <$> runValidation v val
