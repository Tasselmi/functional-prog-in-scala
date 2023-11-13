package rockthejvmYoutube

object AaNewTypes {

  
  
  ////////////////////////////////////////////////////////////////////////////////
  // 1. literal types
  ////////////////////////////////////////////////////////////////////////////////
  val aNum = 3
  val three: 3 = 3 //define a type named 3, and type 3 is subtype of int
  def passNumber(n: Int) = println(s"passNumber: $n")
  def passThree(n: 3) = println(s"passThree: $n")

  passNumber(1)
  passNumber(3)
  //    passThree(1)    //will not compile
  passThree(3)

  
  
  ////////////////////////////////////////////////////////////////////////////////
  // 2. union types
  ////////////////////////////////////////////////////////////////////////////////
  def ambivalentMethod(arg: String | Int) = arg match {
    case i: Int => println("a int")
    case s: String => println("a string")
  }

  type ErrorOr[T] = T | "ERROR"

  def handleResource(file: ErrorOr[java.io.File]) = {

  }

  val stringOrInt = if (43 > 0) "a string" else 43
  val aStringOrInt: String | Int = if (43 > 0) "a string" else 43

  ambivalentMethod("121")
  ambivalentMethod(1)

  
  
  ////////////////////////////////////////////////////////////////////////////////
  // 3. intersection types
  ////////////////////////////////////////////////////////////////////////////////
  trait Camera {
    def takePhoto() = println("snapshot camera")
    def use() = println("use camera")
    def getLens() = println("Canon")
  }

  trait Cellphone{
    def takePhoto() = println("snapshot cellphone")
    def use() = println("use cellphone")
    def makeCall() = println("call")
  }

  def useSmartDevice(sd: Camera & Cellphone) = {
    sd.getLens()
    sd.makeCall()
  }

  class SmartDevice extends Camera with Cellphone {
    override def takePhoto(): Unit = super.takePhoto()  //这样也可以，说明继承是有顺序的，super有所特指
    override def use(): Unit = super.use()
  }

  useSmartDevice(new SmartDevice())

  val dc = new SmartDevice()
  dc.use()
  dc.takePhoto()


  def main(args: Array[String]): Unit = {
  }
}
