package rockthejvmYoutube

object CcTraits {


  ////////////////////////////////////////////////////////////////////////////////
  // 1. trait with contructor arguments
  ////////////////////////////////////////////////////////////////////////////////
  trait Talker(subject: String) {
    def talkTo(another: Talker): String = ""
  }

  class ShowTalker(name: String) extends Talker("rock music")

  class RockFan extends Talker("rock music")
  class RockFanatic extends RockFan with Talker //the second one with no argument



  ////////////////////////////////////////////////////////////////////////////////
  // 2. derived trais will not pass constructor arguments to parent traits
  ////////////////////////////////////////////////////////////////////////////////
  trait BrokenRecord extends Talker
  class AnnoyingFriend extends BrokenRecord with Talker("politics") //class need to pass the parameter


  ////////////////////////////////////////////////////////////////////////////////
  // 3. transparent traits = super trait, will not occur in type infer
  ////////////////////////////////////////////////////////////////////////////////
  transparent trait Paintable
  sealed trait Color
  case object Red extends Color with Paintable
  case object Blue extends Color with Paintable
  case object Green extends Color with Paintable

  val cl1: Color & Paintable & Product = if (true) Red else Blue
  val cl2: Color & Paintable = if (true) Red else Blue
  val cl3: Color = if (true) Red else Blue

  /*
  super traits:

    scala.Product
    java.lang.Comparable
    java.lang.Serializable
  */

  def main(args: Array[String]): Unit = {

  }
}
