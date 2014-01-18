package cern.colt.matrix

object MatrixChecks {
  def checkSizesEqual(a: Matrix1D[_], b: Matrix1D[_]) {
    if (a.size != b.size)
      throw new IllegalArgumentException("Sizes are not equal (" + a.size + " and " + b.size + ")")
  }

  def checkSizeEquals(a: Matrix1D[_], b: Int) {
    if (a.size.toInt != b)
      throw new IllegalArgumentException("Sizes are not equal (" + a.size + " and " + b + ")")
  }

  def checkSizeAtLeast(a: Matrix1D[_], b: Int) {
    if (a.size.toInt < b)
      throw new IllegalArgumentException("Size is not big enough (" + a.size + "); should be at least " + b)
  }

  def checkColumnsEqual(a: Matrix2D[_], b: Int) {
    if (a.columns != b)
      throw new IllegalArgumentException("Columns are not equal (" + a.columns + " and " + b + ")")
  }

  def checkColumnsEqual(a: Matrix2D[_], b: Matrix2D[_]) {
    if (a.columns != b.columns)
      throw new IllegalArgumentException("Columns are not equal (" + a.columns + " and " + b.columns + ")")
  }

  def checkColumnsEqual(a: Matrix2D[_], b: Matrix1D[_]) {
    if (a.columns != b.size.toInt)
      throw new IllegalArgumentException("Columns and size are not equal (" + a.columns + " and " + b.size + ")")
  }

  def checkRowsEqual(a: Matrix2D[_], b: Int) {
    if (a.rows != b)
      throw new IllegalArgumentException("Rows are not equal (" + a.rows + " and " + b + ")")
  }

  def checkRowsEqual(a: Matrix2D[_], b: Matrix2D[_]) {
    if (a.rows != b.rows)
      throw new IllegalArgumentException("Rows are not equal (" + a.rows + " and " + b.rows + ")")
  }

  def checkRowsEqualsSize(a: Matrix2D[_], b: Matrix1D[_]) {
    if (a.rows != b.size.toInt)
      throw new IllegalArgumentException("Rows do not equal size (" + a.rows + " and " + b.size + ")")
  }

  def checkColumnsEqualsSize(a: Matrix2D[_], b: Matrix1D[_]) {
    if (a.columns != b.size.toInt)
      throw new IllegalArgumentException("Columns do not equal size (" + a.columns + " and " + b.size + ")")
  }

  def checkColumnsEqualsRows(a: Matrix2D[_], b: Matrix2D[_]) {
    if (a.columns != b.rows)
      throw new IllegalArgumentException("Columns do not equal rows (" + a.columns + " and " + b.rows + ")")
  }

  def checkColumnsEqualsRows(a: Matrix2D[_]) {
    if (a.columns != a.rows)
      throw new IllegalArgumentException("Columns do not equal rows (" + a.columns + " and " + a.rows + ")")
  }

  def getDensityInfo(a: Matrix[_]): String = {
    if (a == null)
      "null"
    else {
      a match {
        case v: Matrix1D[_] => getDensityInfo(v)
        case m: Matrix2D[_] => getDensityInfo(m)
        case _ => "Matrix type: " + a.getClass.getSimpleName + ", size: " + a.size    // Unsupported details case...
      }
    }
  }

  def getDensityInfo(m: Matrix1D[_]): String = {
    val rows = 1
    val columns = m.size.toInt
    var nonZero = m.numNonZero
    "Matrix type: " + m.getClass.getSimpleName + ", size: " + rows + "x" + columns + ", non-zero: " + nonZero + ", density: " + ((nonZero*100.0)/(rows.toDouble*columns)).formatted("%.2f") + "%"
  }

  def getDensityInfo(m: Matrix2D[_]): String = {
    val rows = m.rows
    val columns = m.columns
    var nonZero = m.numNonZero
    "Matrix type: " + m.getClass.getSimpleName + ", size: " + rows + "x" + columns + ", non-zero: " + nonZero + ", density: " + ((nonZero*100.0)/(rows.toDouble*columns)).formatted("%.2f") + "%"
  }
}

