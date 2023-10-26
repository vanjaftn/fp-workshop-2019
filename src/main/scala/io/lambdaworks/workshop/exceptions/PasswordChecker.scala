package io.lambdaworks.workshop.exceptions

object PasswordChecker {

  def validate(password: String): Either[List[Throwable], String] = {

    (minNumberOfChars(password, password.length), containsUpperCase(password), containsLowerCase(password), containsNumber(password))

    val errorList = List[Throwable]()

    minNumberOfChars(password, password.length).fold(left => left +: errorList, right => password)
    containsUpperCase(password).fold(left => left +: errorList, right => password)
    containsLowerCase(password).fold(left => left +: errorList, right => password)
    containsNumber(password).fold(left => left +: errorList, right => password)

    if (errorList.nonEmpty) {
      Left(errorList)
    } else {
      Right(password)
    }
  }

  private def minNumberOfChars(password: String, length: Int): Either[Throwable, String] =
    if (length < 5) {
    Left(InvalidLength)
  } else {
    Right(password)
  }

  private def containsUpperCase(password: String): Either[Throwable, String] =
    if (!password.exists(letter => letter.isUpper)) {
      Left(MissingUppercase)
    } else {
      Right(password)
    }


  private def containsLowerCase(password: String): Either[Throwable, String] =
    if (!password.exists(letter => letter.isUpper)) {
      Left(MissingLowercase)
    } else {
      Right(password)
    }

  private def containsNumber(password: String): Either[Throwable, String] =
    if (!password.exists(letter => letter.isUpper)) {
      Left(MissingNumber)
    } else {
      Right(password)
    }

  object InvalidLength    extends Throwable("Password must contain at least 5 characters.")
  object MissingUppercase extends Throwable("Password must contain uppercase letter.")
  object MissingLowercase extends Throwable("Password must contain lowercase letter.")
  object MissingNumber    extends Throwable("Password must contain number.")

}
