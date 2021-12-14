package objsets

import scala.annotation.tailrec
import scala.util.Random

class PostSetOwnSuite extends munit.FunSuite {

  val empty = new Empty()
  val aPost = new Post("a", "a body", 20)
  val set2: PostSet = empty.incl(aPost)
  val set3: PostSet = set2.incl(new Post("b", "b body", 20))
  val c = new Post("c", "c body", 7)
  val d = new Post("d", "d body", 9)
  val set4c: PostSet = set3.incl(c)
  val set4d: PostSet = set3.incl(d)
  val set5: PostSet = set4c.incl(d)

  val duplicates: PostSet = empty.incl(aPost).incl(aPost)

  def asSet(posts: PostSet): Set[Post] = {
    var res = Set[Post]()
    posts.foreach(res += _)
    res
  }

  def size(set: PostSet): Int = asSet(set).size

  def createAddRandom(maxAmount: Int): (Int, PostSet) => PostSet = {
    @tailrec
    def addRandom(amount: Int, set: PostSet): PostSet =
      if (amount > maxAmount) set
      else addRandom(amount+1, set.incl(
        new Post(
          Random.alphanumeric.take(6).toString(),
          Random.alphanumeric.take(6).toString(),
          Random.nextInt())))

    addRandom
  }

  //Реалізуйте тест, який перевіряє, що в наборі applePosts існує лише одна публікація з 321 вподобайкою
  test("filter: applePosts has 1 post with 321 likes") {
    assertEquals(size(GoogleVsApple.appleposts.filter(p => p.likes == 321)),1)
  }

  //my variant
  //  Для наборів googlePosts та applePosts створіть об’єднання та відфільтрувати публікації, які містять 321 або 205 вподобайок. Тест має перевірити, що кінцевий результат містить 2 елементи
  test("union and filter: googlePosts and applePosts: 2 filtered elems") {
    assertEquals(
      size(
        GoogleVsApple.appleposts.union(GoogleVsApple.googleposts)
          .filter(p => p.likes == 321 || p.likes == 205)), 2)
  }

  //  Для наборів googlePosts та applePosts створіть об’єднання та відфільтрувати публікації, які містять 321 або 205 вподобайок. Тест має перевірити правильність виконання методу descendingByLike на отриманому результаті
  test("union and filter: googlePosts and applePosts: desc by likes") {
    assertEquals(GoogleVsApple.appleposts.union(GoogleVsApple.googleposts)
      .filter(p => p.likes == 321 || p.likes == 205)
      .descendingByLikes.head.likes, 321)
  }

  //  Реалізуйте тест, що перевіряє, що набори googlePosts та applePosts не містять публікацій з більш ніж 321 вподобанням
  test("union and filter: googlePosts and applePosts: not more that 321 likes") {
    assertEquals(size(GoogleVsApple.appleposts.union(GoogleVsApple.googleposts)
      .filter(p => p.likes > 321)), 0)
  }

  //  Реалізуйте тест, що перевіряє, що набори googlePosts та applePosts не містять від’ємної кількості вподобань
  test("union and filter: googlePosts and applePosts: don't have posts with < 0 likes") {
    assertEquals(size(GoogleVsApple.appleposts.union(GoogleVsApple.googleposts)
      .filter(p => p.likes < 0)), 0)
  }

  //  Перевірте роботу функції filterAcc для порожнього та непорожнього набору
  test("filterAcc") {
    assertEquals(size(empty.filterAcc(p => p.likes > 1, new Empty())), 0)
    assertEquals(size(empty.filterAcc(p => p.likes > 1, set3)), 2)

    assertEquals(size(set5.filterAcc(p => p.likes > 1, new Empty())), 4)
    assertEquals(size(set5.filterAcc(p => p.likes > 1, set4d)), 4)
  }

  //  Перевірте, що перші 2 елементи descendingByLikes відсортовано в порядку спадання
  test("descendingByLikes: two first are sorted") {
    assertEquals(GoogleVsApple.trending.head.likes > GoogleVsApple.trending.tail.head.likes, true)
  }

  //Реалізуйте тести для перевірки роботи функції mostLikeed на об’єднанні  непорожнього та порожнього  наборів
  //  Реалізуйте тести для перевірки роботи функції mostLikeed на об’єднанні порожнього та  непорожнього наборів
  test("mostLiked: on union of empty and non-empty") {
    assertEquals(empty.union(set2).mostLiked.text, "a body")
    assertEquals(set2.union(empty).mostLiked.text, "a body")
    assertEquals(empty.union(set4c).mostLiked.text, "b body")
  }

  //    Реалізуйте тести для перевірки роботи функції mostLikeed на порожньому наборі. Приклад подібного тесту Ви можете знайти за посиланням https://mungingdata.com/scala/testing-munit/
  test("mostLiked: on empty") {
    intercept[NoSuchElementException] {
      empty.mostLiked
    }
  }

  //    Реалізуйте тести для перевірки роботи функції mostLikeed на непорожньому наборі
  test("mostLiked: on non-empty") {
    assertEquals(set5.mostLiked.likes, 20)
    assertEquals(set4d.mostLiked.text, "b body")
  }

  //    Напишіть тест, який перевіряє, що після додавання до непустого набору елементу з найбільшою кількістю вподобань, функція mostLikeed  поверне саме цей елемент
  test("mostLiked: add new biggest") {
    val g = new Post("g", "g body", 100)
    assertEquals(set4c.incl(g).mostLiked, g)
  }

  //    Перевірте, що після виконання функції union  набір даних не містить дублікати елементів
  test("union: no duplicates") {
    assertEquals(size(empty.union(duplicates)), 1)
    assertEquals(size(set2.union(duplicates)), 1)
  }

  //  Напишіть тести, які перевіряють коректність роботи функції contains
  test("contains") {
    assertEquals(set2.contains(aPost), true)
    assertEquals(set5.contains(d), true)
  }

  //  Напишіть тест для функції incl, який перевіряє, що вихідний набір не містить дублікатів
  test("include: no duplicates") {
    assertEquals(size(set2.incl(aPost)), 1)
  }

  //    Напишіть тести для перевірки методів head, tail та isEmpty на пустому списку публікацій. Вам може допомогти наступне посилання https://mungingdata.com/scala/testing-munit/
  test("empty postList: head, tail, isEmpty") {
    intercept[NoSuchElementException] {
      Nil.head
    }
    intercept[NoSuchElementException] {
      Nil.tail
    }

    assertEquals(Nil.isEmpty, true)
  }

  //    Напишіть тест для функції incl, який перевіряє її роботу на пустому наборі
  test("incl: on empty") {
    assertEquals(empty.incl(aPost).contains(aPost), true)
    assertEquals(empty.isInstanceOf[empty.type], true)
    assertEquals(empty.incl(aPost).isInstanceOf[empty.type], false)
  }

  //  Напишіть тест для функції incl, який перевіряє її роботу на непорожньому наборі
  test("incl: on non-empty") {
    assertEquals(empty.incl(aPost).contains(aPost), true)
    assertEquals(empty.isInstanceOf[empty.type], true)
    assertEquals(empty.incl(aPost).isInstanceOf[empty.type], false)
  }

  //  Напишіть тести, які перевіряють коректність роботи функції remove
  test("remove") {
    assertEquals(empty.remove(aPost).isInstanceOf[empty.type], true)
    assertEquals(empty.incl(aPost).remove(aPost).getClass.toString, empty.getClass.toString)
    assertEquals(set4c.remove(c).contains(c), false)
  }

  //  Напишіть тести, які перевіряють коректність роботи функції foreach
  test("foreach: postSet") {
    var counter = 0
    empty.foreach(p => counter += 1)
    assertEquals(counter, 0)

    set5.foreach(p => counter += 1)
    assertEquals(counter, 4)
  }

  //    Напишіть тести, які перевіряють коректність роботи функції foreach на об’єкті типу PostList
  test("foreach: postList") {
    var counter = 0
    set5.descendingByLikes.foreach(p => counter += 1)
    assertEquals(counter, 4)
  }

  //  Перевірте роботу функції filter на наборі даних з більше ніж 200 елементів. Для цього реалізуйте допоміжний метод генерації набору з довільними (рандомними) тестовими даними
  test("filter: 200 elems") {
    val addRandom = createAddRandom(1000)

    addRandom(0, empty).filter(p => p.likes > 0)
  }

//  Перевірте роботу функції union на наборі даних з більше ніж 200 елементів. Для цього реалізуйте допоміжний метод генерації набору з довільними (рандомними) тестовими даними
  test("union: 200 elems") {
    val addRandom = createAddRandom(1000)

    addRandom(0, empty)
      .union(empty)
      .union(
        addRandom(0, empty))
  }

  import scala.concurrent.duration._
  override val munitTimeout: FiniteDuration = 10.seconds

}
