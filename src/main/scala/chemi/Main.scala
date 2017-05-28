package chemi

import collection.immutable.{IndexedSeq ⇒ IxSq}
import collection.parallel.ForkJoinTaskSupport
import java.io.File
import parser._

object Main {

  def run: Unit = {

    val ts = new ForkJoinTaskSupport(
      new scala.concurrent.forkjoin.ForkJoinPool(4))

    def str = getClass.getResourceAsStream("zinc.txt")

    def source = scala.io.Source fromInputStream str getLines
    val lines = source.toArray.par

    def countImpHs(s: String) = smiles(s) fold (_ ⇒ 0, _ foldMap (_.hydrogens))

    lines.tasksupport = ts
    def res = lines map countImpHs sum

    println(res)
  }
}