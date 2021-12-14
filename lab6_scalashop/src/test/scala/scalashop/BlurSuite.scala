package scalashop

import java.util.concurrent._
import scala.collection._
import org.junit._
import org.junit.Assert.assertEquals
import org.scalatest.funsuite.AnyFunSuite
import scalashop.Img


class BlurSuite extends AnyFunSuite {

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)

  test("boxBlurKernel should correctly handle radius 0: 1 pixel") {
    val img = new Img(1, 1)
    img.update(0,0, rgba(125,125,125,125))
    boxBlurKernel(img, 0, 0, 0)
    assert(img.apply(0,0) == rgba(125,125,125,125))
  }

  test("boxBlurKernel should correctly handle radius 0; 25 pixels") {
    val img = new Img(5, 5)

    for (x <- 0 until 5; y <- 0 until 5) img(x, y) =
      if (x == 2 && y == 2) rgba(125,125,125,125) else rgba(x*5, y*10, x*15, y*20)

    img.update(0,0, rgba(125,125,125,125))
    boxBlurKernel(img, 2, 2, 0)
    assert(img.apply(2,2) == rgba(125,125,125,125))
  }

  test("boxBlurKernel should return the correct value on an interior pixel of a 5x5 image with radius 1") {
    val img = new Img(5, 5)
    for (x <- 0 until 5; y <- 0 until 5) img(x, y) = rgba(x+y, x+y, x+y, x+y)

    img.update(0,0, rgba(125,125,125,125))
    boxBlurKernel(img, 2, 2, 1)

    val avg = (9 * 4) / 9 //because of the generated 5x5 square with centre 4

    assert(img.apply(2,2) == rgba(avg, avg, avg, avg))
  }

  test("boxBlurKernel should compute the averages of red, blue, green and alpha channels separately") {
    val img = new Img(5, 5)
    for (x <- 0 until 5; y <- 0 until 5) img(x, y) =
      if (x == 2 && y == 2) rgba(251, 252, 253, 254) else rgba(252, 253, 254, 255)

    img.update(0,0, rgba(125,125,125,125))
    boxBlurKernel(img, 2, 2, 1)

    val notRgba = (rgba(252, 253, 254, 255)*8 + rgba(251, 252, 253, 254)) / 9
    val v = (252*8 + 251) / 9

    assert(img.apply(2,2) == rgba(v, v+1, v+2, v+3))
    assert(img.apply(2,2) != notRgba)
  }

  test("HorizontalBoxBlur blur without the last strip") {
    val img = new Img(5, 5)
    for (x <- 0 until 5; y <- 0 until 5) img(x, y) = rgba(1,1,1,1)

    val dst = new Img(5, 5)
    HorizontalBoxBlur.blur(img, dst, 0, 4, 5)

    for (x <- 0 until 4; y <- 0 until 5) {
      assert(dst.apply(x,y) != 0)
    }
  }

  test("HorizontalBoxBlur parBlur should not forget the last strip") {
    val img = new Img(5, 5)
    for (x <- 0 until 5; y <- 0 until 5) img(x, y) = rgba(1,1,1,1)

    val dst = new Img(5, 5)
    HorizontalBoxBlur.parBlur(img, dst, 2, 1)

    for (x <- 0 until 5; y <- 0 until 5) {
      assert(dst.apply(x,y) != 0)
    }
  }

  test("VerticalBoxBlur blur without the last strip") {
    val img = new Img(5, 5)
    for (x <- 0 until 5; y <- 0 until 5) img(x, y) = rgba(1,1,1,1)

    val dst = new Img(5, 5)
    VerticalBoxBlur.blur(img, dst, 0, 4, 5)

    for (x <- 0 until 5; y <- 0 until 4) {
      assert(dst.apply(x,y) != 0)
    }
  }

  test("VerticalBoxBlur parBlur should not forget the last strip") {
    val img = new Img(5, 5)
    for (x <- 0 until 5; y <- 0 until 5) img(x, y) = rgba(1,1,1,1)

    val dst = new Img(5, 5)
    VerticalBoxBlur.parBlur(img, dst, 2, 5)

    for (x <- 0 until 5; y <- 0 until 5) {
      assert(dst.apply(x,y) != 0)
    }
  }

  //  HorizontalBoxBlu / VerticalBoxBlur parBlur with 32 tasks should modify each pixel
  test("HorizontalBoxBlu / VerticalBoxBlur parBlur with 2 tasks should modify each pixel") {
    val img = new Img(5, 5)
    for (x <- 0 until 5; y <- 0 until 5) img(x, y) = rgba(1,1,1,1)

    val dst1 = new TestImg(5, 5)
    VerticalBoxBlur.parBlur(img, dst1, 2, 5)
    assert(dst1.checkModifiedOnce())

    val dst2 = new TestImg(5, 5)
    HorizontalBoxBlur.parBlur(img, dst2, 2, 5)
    assert(dst2.checkModifiedOnce())
  }

  test(" HorizontalBoxBlu / VerticalBoxBlur with radius 1 and 4 tasks should correctly blur the entire 3x3 image") {
    val img = new Img(3, 3)
    for (x <- 0 until 3; y <- 0 until 3) img(x, y) = rgba(1,1,1,1)

    val dst1 = new TestImg(3,3)
    VerticalBoxBlur.parBlur(img, dst1, 4, 1)
    assert(dst1.checkModifiedOnce())

    val dst2 = new TestImg(3,3)
    HorizontalBoxBlur.parBlur(img, dst2, 4, 1)
    assert(dst2.checkModifiedOnce())
  }

  class TestImg(w: Int, h: Int) extends Img(w, h) {
    private val _upd = new Array[Int](w*h)
    def updNum: Array[Int] = _upd

    override def update(x: RGBA, y: RGBA, c: RGBA): Unit = {
      _upd(y * width + x) += 1
      super.update(x, y, c)
    }

    def checkModifiedOnce(): Boolean = _upd.forall(_ == 1)
  }
}
