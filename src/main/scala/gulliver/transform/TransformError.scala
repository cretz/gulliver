package gulliver.transform

// TODO: Stop using AnyRef when we put common trait on all ast nodes
case class TransformError(message: String, ast: Option[AnyRef] = None) extends RuntimeException(message) {

}

object TransformError {
  def apply(message: String, ast: AnyRef): TransformError = TransformError(message, Some(ast))
}