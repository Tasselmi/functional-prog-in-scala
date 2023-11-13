import $ivy.`org.scala-lang.modules::scala-parallel-collections:1.0.4`

import scala.collection.parallel.CollectionConverters._
import os.ProcessOutput

def process(classFile: os.Path, bytecodeFile: os.Path): Unit = {
  println(s"Processing ${classFile}")
  val args =
    Seq(
      "javap",
      "-v",
      "-c",
      classFile.toNIO.toAbsolutePath().toString()
    )

  os.proc(args)
    .call(
      stdout = ProcessOutput.makePathRedirect(bytecodeFile),
      stderr = os.Inherit
    )
}

@main
def main(classesFolder: os.Path, bytecodeFolder: os.Path): Unit = {
  println("\nhello, I am the entrance of this program...\n")
  println(s"classesFolder: $classesFolder, bytecodeFolder: $bytecodeFolder")

  interp.watch(classesFolder)

  os.makeDir.all(bytecodeFolder)

  os.walk(bytecodeFolder).filter(_.ext == "bytecode").foreach { p =>
    os.remove(p)
  }

  val allClassfiles = os.walk(classesFolder).filter(_.ext == "class")


  allClassfiles.zip(allClassfiles.map(_.baseName + ".bytecode")).par.foreach {
    case (absPath, filename) =>
      val target = bytecodeFolder / filename

      process(absPath, target)
  }
}