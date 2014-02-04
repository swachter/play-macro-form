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

    //
    //
    //

    def fill(name: Name, model: FV) = FS(
      f1.doFill(name + "f1", model.f1),
      f2.doFill(name + "f2", model.f2),
      f3.doFill(name + "f3", model.f3),
      f4.doFill(name + "f3", model.f4)
    )

    def parse(name: Name, view: Map[String, Seq[String]]) = FS(
      f1.doParse(name + "f1", view),
      f2.doParse(name + "f2", view),
      f3.doParse(name + "f3", view),
      f4.doParse(name + "f4", view)
    )

    case class FV(
                  f1: Int,
                  f2: Option[Int],
                  f3: Int,
                  f4: Seq[Int]
                  )

    case class FS[C1 <: Constraints[Int, _], C2 <: Constraints[Int, _], C3 <: Constraints[Int, _], C4 <: Constraints[Int, _]](
                                  f1: FieldState[Int,C1],
                                  f2: FieldState[Option[Int],C2],
                                  f3: FieldState[Int,C3],
                                  f4: FieldState[Seq[Int],C4]
                                  ) extends State[FV] {
      override def hasErrors: Boolean = !errors.isEmpty || Seq(f1, f2, f3).exists(_.hasErrors)
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

    def fill(name: Name, model: FV) = FS(
      g1.fill(name + "g1", model.g1),
      g2.fill(name + "g2", model.g2),
      g3.doFill(name + "g3", model.g3)
    )

    def parse(name: Name, view: Map[String, Seq[String]]) = FS(
      g1.parse(name + "g1", view),
      g2.parse(name + "g2", view),
      g3.doParse(name + "g3", view)
    )

    case class FV(
                   g1: F.FV,
                   g2: F.FV,
                   g3: Int
                   )

    case class FS[C1 <: Constraints[Int, _]](
            g1: State[F.FV],
            g2: State[F.FV],
            g3: FieldState[Int,C1]
            ) extends State[FV] {
      override def hasErrors: Boolean = !errors.isEmpty || Seq(g1, g2, g3).exists(_.hasErrors)
      override def model: FV = FV(g1.model, g2.model, g3.model)
    }

  }

  test("form") {

    val fs = F.fill(new Name(""), F.FV(1, Some(2), 3, Seq(4, 5)))

    println(fs.f1.model)
    println(fs.f2.model)
    println(fs.f3.model)
    println(fs.f4.model)

    println(fs)

    val simpleRenderer: FieldState[_, _] => String =
        state => s"simple renderer - state: $state"

    val enumRenderer: FieldState[_, Constraints[_, CState { type EN = Set }]] => String =
        state => s"enum renderer - state: $state; enum: ${state.constraints.en.get}"

    println(simpleRenderer(fs.f1))
    println(enumRenderer(fs.f3))

    val gs = G.fill(new Name(""), G.FV(fs.model, fs.model, 9))

    println(gs)
  }

}

