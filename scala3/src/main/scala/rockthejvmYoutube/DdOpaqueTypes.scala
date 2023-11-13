package rockthejvmYoutube

object DdOpaqueTypes {

  //case class Name(value: String)

  object SocialNetwork {
    opaque type Name = String //opaque only works in current scope

    ////////////////////////////////////////////////////////////////////////////////
    // 1. use companion object to connect Name and String
    ////////////////////////////////////////////////////////////////////////////////
    object Name {
      def fromString(s: String): Option[Name] =
        if (s.isEmpty || s.charAt(0).isLower) then None
        else Some(s)
    }


    ////////////////////////////////////////////////////////////////////////////////
    // 2. use extension methods
    ////////////////////////////////////////////////////////////////////////////////
    extension (n: Name) {
      def length: Int = n.length  //on string class
    }
  }

  //outside the scope, Name != String
  import SocialNetwork.Name
//  val name: Name = "liangfan" //will not compile


  object Graphics {
    opaque type Color = Int
    opaque type ColorFilter <: Color = 100 | 200 | 300

    val Red: Color = 0xFF000000
    val halfTransparency: ColorFilter = 200
  }

  import Graphics.*
  case class OverlayFilter(c: Color)

  val fadeLayer = OverlayFilter(halfTransparency)


  def main(args: Array[String]): Unit = {
    val nameOpt = Name.fromString("Liang Fan")
    nameOpt.foreach(println)
//    nameOpt.map(_.charAt(0)).foreach(println) //can not use string method, name is of Name type

    nameOpt.map(_.length).foreach(println)  //Name type has length method with extension
  }
}
