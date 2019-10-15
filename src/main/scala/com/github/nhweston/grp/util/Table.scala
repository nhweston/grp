package com.github.nhweston.grp.util

case class Table[Row, Col, Entry] (
    rows: Seq[Row],
    cols: Seq[Col],
    entries: (Row, Col) => Entry
) {

    override def toString: String = {
        val strEntries: Map[(Row, Col), String] =
            rows.flatMap {
                row => cols.map (col => (row, col) -> entries (row, col) .toString)
            } .toMap
        val strRows: Map[Row, String] = rows.map (row => row -> row.toString) .toMap
        val strCols: Map[Col, String] = cols.map (col => col -> col.toString) .toMap
        val widths: Map[Col, Int] =
            cols.map {
                col => {
                    val lengths = strCols (col) .length +: rows.map (row => strEntries ((row, col)) .length)
                    col -> (lengths.max + 1)
                }
            } .toMap
        val widthHeader = strRows.values.map (_.length) .max + 1
        val builder = new StringBuilder ()
        builder ++= (" " * widthHeader)
        for (col <- cols) builder ++= s"%${widths (col)}s" .format (strCols (col))
        for (row <- rows) {
            builder += '\n'
            builder ++= s"%${widthHeader}s" .format (strRows (row))
            for (col <- cols) builder ++= s"%${widths (col)}s" .format (strEntries ((row, col)))
        }
        builder.result ()
    }

}
