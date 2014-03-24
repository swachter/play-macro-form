# play-ext

This project contains some extensions for Play. The project contains the following subprojects:

1. **core**: a library that contains the "Form Definition" and "Styling" extensions.
2. **play-mod**: a Play module that can be included in Play applications in order to use the "Form
Rendering" extension.
3. **play-app**: an example play application

(The **plugin** subproject is used during the built only in order to check the instantiation of value classes.)

## Improved Form Handling

The improved form handling allows to easily define forms and their fields. A field definition is based on the type of the
corresponding field value and allows to add further constraints. Various features of a field are made explicit by the type
system. For example the multiplicity of a field value (exactly one, zero or one, zero or more) is captured by a phantom type.
This allows to ensure that the a field can be input only by a suitable input mechanism (e.g. a drop down list
is suitable only if the maximum occurrence of a value is one).

During form processing string values are parsed into a so called **FormState**. The form state of a form is a case class
that has members that correspond to the members of the form definition. Two cases can be distinguished:

* For each field the form state member is a so called **FieldState** that contains the string representation of its value,
any parse or validation errors, and the so called field value (if the parse was successful). The field value has the
type that is specified in the field definition.
* For each nested form the form state member is the corresponding **FormState**.

If a form state contains no errors then it allows to access the so called form value. The form value is a case class
with members that correspond to the fields and nested forms of the form. Again, these members are strongly typed in
order to allow type safe form processing.

### Form Definition

*Note: In order to use the "Form Definition" extension the macro paradise plugin must be used.*

Forms can easily be defined by using the @Form annotation on objects that contain field definitions. The @Form annotation
is a macro annotation that enriches the annotated object in the following ways:

* A case class FS that represents the form state
* A case class FV that represents the form value
* A parse method that parses a map of string values into a form state
* A fill method that fills a form value into a form state

Example of a simple form definition:

    @Form
    object F {
      val f1 = field[Int].enum(Seq(2,3,4))
      val f2 = field[Option[Int]]
      val f3 = field[Seq[Int]].enum(Seq(2,3,4))
    }

### Form Rendering

HTML form fields can easily be rendered by invoking suitable rendering methods on field state members. For example if
the form given above is used and a template has a corresponding form state `fs` as its parameter then HTML form fields
for all three fields can be rendered by the following code:

    fs.f1.selectList
    fs.f2.inputText
    fs.f3.selectList

Note that the first and the third line use the same rendering method `selectList`. In the first line a drop down list
is rendered (allowing a single input) whereas in the third line a selection list is rendered (allowing multiple inputs).

At the moment form rendering supports to output forms with a structure and CSS style classes that are aligned to Bootstrap 3.
Yet, it would be possible to support different outputs

## Styling (HTML Output Adaption)

In order to ease the adaption of the HTML output of templates functionality is provided that allows to easily define, adapt, and output
sets of attributes. This functionality replaces difficult to write and maintain logic inside templates that would be
necessary in order to allow specifying default values for attributes or augmenting the value of attributes.

The key concept is the *attribute set* which is represented by the **Attrs** type. The Attrs type simply is
an alias for a `Map[String, Set[String]]`. An attribute set maps attribute names into a set of values. A set is
used for the value because some attributes (most prominently the `class` attribute) can have a set of values. Most attributes
however have a set with a single entry as their value. On top of that a **Style** is defined to be a `Map[String, Attrs]`,
i.e. a style contains several attribute sets that are identified by string valued keys.

In order to adapt the HTML output of a certain realm (e.g. the output of HTML forms) several attribute sets may be
necessary. For example there may be an attribute set that is used for the `<form>` element containing values for the `class`, `method` and
`action` attributes and another attribute set that is used for the `<input>` element containing values for the `class` and `value`
attributes. In order to identify these attribute sets the concept of a **StyledItem** is introduced. A styled item allows
to access a specific attribute set from a style and supports tweaking its attribute set by an internal DSL.

Attributes are described by **AttrDesc** instances. An attribute description contains the name of the attribute and describes
if an attribute has a single value or if it can have a set of values. A number of attribute description is provided for
common HTML attributes like the `class` or `name` attribute. The identifier for these attribute descriptions have a
`_@` suffix in order to avoid name clashes and to remind of attributes.

If a template has an implicit style parameter and `input` is a **StyledItem** then the following code outputs an `<input>` element
with all its attributes:

    <input @Html(input)/>

Additionally, if the `<input>` element should have the CSS class `css-class` and a default value of "please enter a value"
for the placeholder attribute then the following code can be used:

    <input @Html(input += (class_@, "css-class") ~= (placeholder_@, "please enter a value"))/>

