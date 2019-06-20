module Components.Form where

import Prelude

import Data.Either (Either(..))
import Data.Lens (Lens', lens)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Lumi.Components.Form (FormBuilder, Validated, Validator)
import Lumi.Components.Form as F
import Lumi.Components.LabeledField (RequiredField(..))

newtype EmailAddress = EmailAddress NonEmptyString

getString :: EmailAddress -> NonEmptyString
getString (EmailAddress s) = s

newtype Registration = Registration
  { email :: EmailAddress
  , password :: NonEmptyString
  }

type RegistrationFormData =
  { email :: Validated String
  , password :: Validated String
  , passwordConfirmation :: Validated String
  }

registrationForm :: FormBuilder { readonly :: Boolean } RegistrationFormData Registration
registrationForm = ado
  email <-
    F.indent "Email" Required
    $ F.focus _email
    $ F.validated (isValidEmail "Email")
    $ F.validated (F.nonEmpty "Email")
    $ F.textbox
  password <-
    F.indent "Password" Required
    $ F.focus _password
    $ F.validated (F.nonEmpty "Password")
    $ F.passwordBox
  _ <- F.withValue \{ password } ->
    F.indent "Password confirmation" Required
    $ F.focus _passwordConfirmation
    $ F.validated (passwordsMatch password)
    $ F.passwordBox
  in
    Registration
      { email
      , password
      }

-- Utils {{{

_email :: forall a r. Lens' { email :: a | r } a
_email = lens _.email _{ email = _ }

_password :: forall a r. Lens' { password :: a | r } a
_password = lens _.password _{ password = _ }

_passwordConfirmation :: forall a r. Lens' { passwordConfirmation :: a | r } a
_passwordConfirmation = lens _.passwordConfirmation _{ passwordConfirmation = _ }

isValidEmail :: String -> Validator NonEmptyString EmailAddress
isValidEmail name s =
  if NES.contains (NES.Pattern "@") s then
    Right $ EmailAddress s
  else
    Left $ name <> " is not a valid email address."

passwordsMatch :: String -> String -> Validator NonEmptyString NonEmptyString
passwordsMatch a b
  | a == b = Right b
  | otherwise = Left "passwords do not match"

foreign import unsafeRegistrationToString :: Registration -> String

-- }}}
