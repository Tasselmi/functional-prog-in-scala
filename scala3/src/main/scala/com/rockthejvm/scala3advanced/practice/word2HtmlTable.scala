package com.rockthejvm.scala3advanced.practice

import scala.io.Source

object word2HtmlTable {

  val raw = """
    ------------------------------------------------------------------------------------------------------------------------------------------------
  数据格式                含义及解释                                                                                 是否支持输入   是否支持输出
  ----------------------- ------------------------------------------------------------------------------------------ -------------- --------------
  Avro                    Apache Avro是一种远程过程调用和数据序列化的框架，可以为数据持久化（如写入文件）提供序列化格式                                                                                     是             是

  CSV                     逗号分隔符文件                                                                             是             是

  CSVWithNames            带字段名的逗号分隔符文件                                                                   是             是

  JSON                    将所有数据行解释为1个JavaScript对象                                                        否             是

  JSONEachRow             将每行数据解释为1个JavaScript对象                                                          是             是

  ORC                     一种优化的列式存储格式                                                                     是             是

  Parquet                 一种面向列存存储的文件格式                                                                 是             是

  Pretty                  美化后的终端输出                                                                           否             是

  PrettyCompact           美化后的终端输出，更加紧凑. ClickHouse官方客户端默认的的输出格式                           否             是

  Protobuf                Google开源的远程过程调用和数据序列化的框架，可以为数据持久化（如写入文件）提供序列化格式   是             是

  TabSeparated            Tab分隔符文件                                                                              是             是

  TabSeparatedWithNames   带字段名的Tab分隔符文件                                                                    是             是

  Values                  元组格式，官方客户端插入数据的默认格式                                                     是             是

  XML                     可扩展标记语言格式，常用于配置文件等                                                       否             是
  ------------------------------------------------------------------------------------------------------------------------------------------------
  """

  extension (str: String) {

    def transferLogic1 = str
      .split("\n")
      .filterNot(_.contains("--"))
      .filter(_.trim.nonEmpty)
      .zipWithIndex
      .map { (line, idx) =>
        if idx == 0 then
          line
            .split("\\s{2,}")
            .filter(_.trim.nonEmpty)
            .map(a => s"<th>$a</th>")
            .mkString("<tr>", "\n", "</tr>")
        else
          line
            .split("\\s{2,}")
            .filter(_.trim.nonEmpty)
            .map(a => s"<td>$a</td>")
            .mkString("<tr>", "\n", "</tr>")
      }
      .mkString("<table border=\"1\">", "", "</table>")

    def transferLogic2 = str
      .split("\n")
      .filterNot(_.contains("--"))
      .filterNot(_.contains("=="))
      .filter(_.trim.nonEmpty)
      .zipWithIndex
      .map { (line, idx) =>
        if idx == 0 then
          line
            .replaceAll("|", " ")
            .split("\\s{2,}")
            .filter(_.trim.nonEmpty)
            .map(a => s"<th>$a</th>")
            .mkString("<tr>", "\n", "</tr>")
        else
          line
            .replaceAll("|", " ")
            .split("\\s{2,}")
            .filter(_.trim.nonEmpty)
            .map(a => s"<td>$a</td>")
            .mkString("<tr>", "\n", "</tr>")
      }
      .mkString("<table border=\"1\">", "", "</table>")

  }

  def main(args: Array[String]): Unit = println(raw.transferLogic1)
}
