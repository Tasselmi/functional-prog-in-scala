package com.rockthejvm.scala3advanced.practice

import scala.io.Source

object word2HtmlTable {

  val raw = """
+-----------+-----------+-----------+-----------+-----------+-----------+
| 视图类型  | 英文缩写  | 英文全称  | 数据存储  | 监听行为  | 实际业务意义 |
+===========+===========+===========+===========+===========+===========+
| 虚拟视图  | vv        | virtual   | 虚拟视图不存储数据 | 每次查询虚拟视图， | 可以将虚拟视图理解 |
|           |           |           | ，其存储的是SQL | 相当于在源表基础上 | 为一张不存储数据的 |
|           |           | view      | 计算逻辑  | 执行了一次SQL查 | 中间表，对于经常重 |
|           |           |           |           | 询，因此源表的所有 | 复使用的SQL计算 |
|           |           |           |           | 变化都会体现在虚拟 | 逻辑可以建成虚拟视 |
|           |           |           |           | 视图中    | 图        |
+-----------+-----------+-----------+-----------+-----------+-----------+
| 物化视图  | mv        | materiali | 存储数据  | 只会监听数据插入， | 物化视图可以用来监 |
|           |           | zed       |           | 源表数据修订和删除 | 听流数据，如kaf |
|           |           | view      |           | 不会体现在物化视图 | ka中的消息数据； |
|           |           |           |           | 中。对于数据插入， | 或存储聚合计算的中 |
|           |           |           |           | 其会物化每批次插入 | 间结果（先将每个批 |
|           |           |           |           | 的数据，而不是所有 | 次的数据聚合结果计 |
|           |           |           |           | 插入数据的最终结果 | 算出来存储） |
+-----------+-----------+-----------+-----------+-----------+-----------+
| 分布式表  | lv        | live view | 存储数据  | 只会监听数据插入， | 可以取代实时计算引 |
|           |           |           |           | 源表数据修订和删除 | 擎来制作实时看板； |
|           |           |           |           | 不会体现在实况视图 | 可以用来监听数据变 |
|           |           |           |           | 中。存储的数据记录 | 化情况并推送消息提 |
|           |           |           |           | 均是记录产生时间点 | 醒避免不间断的轮询 |
|           |           |           |           | 前所有历史数据的全 |  |
|           |           |           |           | 量计算结果 |          |
+-----------+-----------+-----------+-----------+-----------+-----------+                                                                                               增加限制条件可能影响数据写入性能。不满足限制条件的数据无法插入数据表 
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

  def main(args: Array[String]): Unit = println(raw.transferLogic2)
}
