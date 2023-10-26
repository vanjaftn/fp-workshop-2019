package io.lambdaworks.workshop.exceptions

object PasswordChecker {

  def validate(password: String): Either[Seq[Throwable], String] = {

    val criteria = Seq(minNumberOfChars(password, password.length), containsNumber(password), containsLowerCase(password), containsUpperCase(password))

    minNumberOfChars(password, password.length) match {
      case Left(_) => Left(criteria.collect{ case Left(ex) => ex})
      case Right(_) => containsUpperCase(password) match {
        case Left(_) => Left(criteria.collect{ case Left(ex) => ex})
        case Right(_) => containsLowerCase(password) match {
          case Left(_) => Left(criteria.collect{ case Left(ex) => ex})
          case Right(_) => containsNumber(password) match {
            case Left(_) => Left(criteria.collect{ case Left(ex) => ex})
            case Right(_) => Right(password)
          }
        }
      }
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
    if (!password.exists(letter => letter.isLower)) {
      Left(MissingLowercase)
    } else {
      Right(password)
    }

  private def containsNumber(password: String): Either[Throwable, String] =
    if (!password.exists(letter => letter.isDigit)) {
      Left(MissingNumber)
    } else {
      Right(password)
    }

  object InvalidLength    extends Throwable("Password must contain at least 5 characters.")
  object MissingUppercase extends Throwable("Password must contain uppercase letter.")
  object MissingLowercase extends Throwable("Password must contain lowercase letter.")
  object MissingNumber    extends Throwable("Password must contain number.")

}
