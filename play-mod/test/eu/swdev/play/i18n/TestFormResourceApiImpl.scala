package eu.swdev.play.i18n

import eu.swdev.i18n.Resource
import play.api.Application
import eu.swdev.play.form.StdFormResourceApi

@Resource(resourcePath = "form/form-resource", locales = List("de_DE"))
class TestFormResourceApiImpl(app: Application) extends StdFormResourceApi