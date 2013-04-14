package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(4)
    val s6 = singletonSet(6)
    val s8 = singletonSet(8)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
      assert(contains(s2, 2), "Singleton")
      assert(contains(s3, 3), "Singleton")
      assert(!contains(s1, 2), "Singleton")
      assert(!contains(s1, 3), "Singleton")
      assert(!contains(s2, 1), "Singleton")
      assert(!contains(s2, 3), "Singleton")
      assert(!contains(s3, 1), "Singleton")
      assert(!contains(s3, 2), "Singleton")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
      val newS = union(s, s3)
      assert(contains(newS, 1), "Union 4")
      assert(contains(newS, 2), "Union 5")
      assert(contains(newS, 3), "Union 6")
    }
  }

  test("intersection of two tests") {
	  new TestSets {
      val s = union(s1, s2)
      assert(contains(intersect(s, s1), 1), "intersection 1")
      assert(contains(intersect(s, s2), 2), "intersection 2")
      assert(!contains(intersect(s, s2), 3), "no intersection 3")
    }
  }

  test("diff of two tests") {
	  new TestSets {
      val s12 = union(s1, s2)
      assert(contains(diff(s12, s1), 2), "diff 1")
      assert(!contains(diff(s12, s1), 1), "diff 2")
      val s23 = union(s2, s3)
      assert(contains(diff(s23, s2), 3), "diff 3")
      assert(contains(diff(s23, s3), 2), "diff 3")
      assert(!contains(diff(s23, s2), 2), "diff 4")
    }
  }

  test("filter set") {
    new TestSets {
      val s12 = union(s1, s2)
      val s123 = union(s12, s3)
      def p(x: Int) = x == 2
      assert(contains(filter(s123, p), 2), "filter 1")
      assert(!contains(filter(s123, p), 1), "filter 2")
      assert(!contains(filter(s123, p), 3), "filter 3")
    }
  }

  test("forall in set") {
    new TestSets {
      val s24 = union(s2, s4)
      val s246 = union(s24, s6)
      def p(x: Int) = x % 2 == 0
      def n(x: Int) = x % 2 == 1
      assert(forall(s246, p), "forall 1")
      assert(!forall(s246, n), "forall 2")
    }
  }

  test("exists in set") {
    new TestSets {
      val s12 = union(s1, s2)
      val s123 = union(s12, s3)
      def p(x: Int) = x % 2 == 0
      def n(x: Int) = x % 2 == 1
      assert(exists(s123, p), "exists 1")
      assert(exists(s123, n), "exists 2")
    }
  }

  test("map of set") {
    new TestSets {
      val s12 = union(s1, s2)
      val s123 = union(s12, s3)
      def p(x: Int) = x * x
      assert(contains(map(s123, p), 1), "map 1")
      assert(contains(map(s123, p), 4), "map 2")
      assert(contains(map(s123, p), 9), "map 3")
      assert(!contains(map(s123, p), 2), "map 4")
      assert(!contains(map(s123, p), 3), "map 5")
    }
  }


}
