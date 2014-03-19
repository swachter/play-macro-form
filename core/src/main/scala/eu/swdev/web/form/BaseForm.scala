package eu.swdev.web.form

/**
 * This trait is automatically implemented by objects that are annotated with the @Form annotation.
 */
trait BaseForm {

  /**
   * The type of the value that can be extracted from a form state if it has no errors.
   */
  type FV

  /**
   * The type of the form state.
   */
  type FS <: FormState[FV]

  /**
   * Parses the string representation of form fields and returns a corresponding form state.
   *
   * @param view
   * @param validationArg
   * @param name The default value of the name argument is overridden by the default value in subclasses. Form objects
   *             supply their object name as the default.
   * @return
   */
  def parse(view: Map[String, Seq[String]], validationArg: Validation = WithValidation, name: Name = Name.empty): FS

  /**
   * Fills a form state with the specified form value.
   *
   * @param model
   * @param validationArg
   * @param name The default value of the name argument is overridden by the default value in subclasses. Form objects
   *             supply their object name as the default.
   * @return
   */
  def fill(model: FV, validationArg: Validation = WithValidation, name: Name = Name.empty): FS

}
