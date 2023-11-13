package rockthejvmYoutube

object BbEnums {

  enum Permissions {
    case READ, WRITE, EXEC, NONE
  }

  val read = Permissions.READ


  enum PersmissonsWithBits(val bits: Int) {
    case READ extends PersmissonsWithBits(4)
    case WRITE extends PersmissonsWithBits(2)
    case EXEC extends PersmissonsWithBits(1)
    case NONE extends PersmissonsWithBits(0)

    def toBinary: String = Integer.toBinaryString(bits)
  }

  val read2 = PersmissonsWithBits.READ
  val bitString = read2.bits
  val hexString = read2.toBinary

  println(s"$read2, $bitString, $hexString")

  val first = Permissions.WRITE.ordinal
  val alls = Permissions.values
  alls.foreach(println)

  val read3 = Permissions.valueOf("READ")
  val read4 = Permissions.fromOrdinal(3)
  println(s"$read3, $read4")

  def main(args: Array[String]): Unit = {

  }
}
