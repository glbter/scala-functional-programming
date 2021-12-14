package scalashop

import org.scalameter._

import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable

object HorizontalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns := 5,
    Key.exec.maxWarmupRuns := 10,
    Key.exec.benchRuns := 10,
    Key.verbose := true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      HorizontalBoxBlur.blur(src, dst, 0, height, radius)
    }

    val numTasks = 32
    val partime = standardConfig measure {
      HorizontalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime")
    println(s"speedup: ${seqtime.value / partime.value}")
  }
}

/** A simple, trivially parallelizable computation. */
object HorizontalBoxBlur extends HorizontalBoxBlurInterface {

  /** Blurs the rows of the source image `src` into the destination image `dst`,
   *  starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each row, `blur` traverses the pixels by going from left to right.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    // TODO implement this method using the `boxBlurKernel` method
    for (w <- from until end; h <- 0 until src.height) {
      dst.update(w, h, boxBlurKernel(src, w, h, radius))
    }
  }

  /** Blurs the rows of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  rows.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    // TODO implement using the `task` construct and the `blur` method
    val tasks = math.min(src.width, numTasks)
    val widthPerTask = src.width / tasks
    val columns = (0 to src.width+widthPerTask)
      .by(widthPerTask)
        .map(it => if (it >= src.width) src.width else it)
        .distinct

      columns
      .zip(columns.tail)
      .toList.par
      .map(it => task {blur(src, dst, it._1, it._2, radius) })
      .map(_.join())
      .seq
    // https://stackoverflow.com/a/38717191 this method is said to be more effective
    // unfortunately, didn't find anything like waitAll()

    // less effective method
    // taskPairs.map(it => task {blur(src, dst, it._1 ,it._2, radius) }).foreach(_.join())
  }

}
