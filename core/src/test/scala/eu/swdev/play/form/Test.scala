package eu.swdev.play.form

import org.scalatest.FunSuite

/**
  */
class Test extends FunSuite {

  object F {
    
    val f1 = field[Int]
    val f2 = field[Int, Option].lt(7)
    val f3 = field[Int].enum(Seq(3, 4, 5))
    val f4 = field2[Seq[Int]]

    def validate(fs: WFS): Unit = {
      if (fs.f1.model % 2 == 0) {
        fs.f1.addError("number must not be even")
      }
    }

    //
    //
    //

    type WFS = FS[_, _, _, _]

    def doFill(name: Name, model: FV) = {
      val fs = FS(
        f1.doFill(name + "f1", model.f1),
        f2.doFill(name + "f2", model.f2),
        f3.doFill(name + "f3", model.f3),
        f4.doFill(name + "f3", model.f4)
      )
      if (!fs.hasFieldErrors) {
        validate(fs)
      }
      fs
    }

    def fill(model: FV) = doFill(Name.empty, model)

    def doParse(name: Name, view: Map[String, Seq[String]]) = FS(
      f1.doParse(name + "f1", view),
      f2.doParse(name + "f2", view),
      f3.doParse(name + "f3", view),
      f4.doParse(name + "f4", view)
    )

    def parse(view: Map[String, Seq[String]]) = doParse(Name.empty, view)

    case class FV(
                  f1: Int,
                  f2: Option[Int],
                  f3: Int,
                  f4: Seq[Int]
                  )

    case class FS[CS1 <: CState, CS2 <: CState, CS3 <: CState, CS4 <: CState](
                                  f1: FieldState[Int,Id,CS1],
                                  f2: FieldState[Int,Option,CS2],
                                  f3: FieldState[Int,Id,CS3],
                                  f4: FieldState[Int,Seq,CS4]
                                  ) extends State[FV] {
      def hasFormErrors: Boolean = !errors.isEmpty || Seq[State[_]](f1, f2, f3).exists(_.hasFormErrors)
      def hasFieldErrors: Boolean = Seq[State[_]](f1, f2, f3).exists(_.hasFieldErrors)
      override def model: FV = FV(f1.model, f2.model, f3.model, f4.model)
    }

  }

  object G {

    val g1 = F
    val g2 = F
    val g3 = field[Int]

    //
    //
    //

    def doFill(name: Name, model: FV) = FS(
      g1.doFill(name + "g1", model.g1),
      g2.doFill(name + "g2", model.g2),
      g3.doFill(name + "g3", model.g3)
    )

    def fill(model: FV) = doFill(Name.empty, model)

    def doParse(name: Name, view: Map[String, Seq[String]]) = FS(
      g1.doParse(name + "g1", view),
      g2.doParse(name + "g2", view),
      g3.doParse(name + "g3", view)
    )

    def parse(view: Map[String, Seq[String]]) = doParse(Name.empty, view)

    case class FV(
                   g1: F.FV,
                   g2: F.FV,
                   g3: Int
                   )

    case class FS[CS1 <: CState](
            g1: State[F.FV],
            g2: State[F.FV],
            g3: FieldState[Int,Id,CS1]
            ) extends State[FV] {
      def hasFormErrors: Boolean = !errors.isEmpty || Seq(g1, g2, g3).exists(_.hasFormErrors)
      def hasFieldErrors: Boolean = Seq(g1, g2, g3).exists(_.hasFieldErrors)
      override def model: FV = FV(g1.model, g2.model, g3.model)
    }

  }

  test("form") {

    val fs = F.fill(F.FV(1, Some(2), 3, Seq(4, 5)))

    println(fs.f1.model)
    println(fs.f2.model)
    println(fs.f3.model)
    println(fs.f4.model)

    println(fs)

    def simpleRenderer[B[_]]: FieldState[_, B, _] => String =
        state => s"simple renderer - state: $state"

    def enumRenderer[B[_]]: FieldState[_, B, CState { type EN = Set }] => String =
        state => s"enum renderer - state: $state; enum: ${state.constraints.en.get}"

    println(simpleRenderer(fs.f1))
    println(enumRenderer(fs.f3))

    val gs = G.fill(G.FV(fs.model, fs.model, 9))

    println(gs)
  }

}

