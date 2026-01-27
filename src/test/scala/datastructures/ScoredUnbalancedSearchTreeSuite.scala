package datastructures

import org.scalatest.funsuite.AnyFunSuite
import datastructures.UnbalancedSearchTree.*


class ScoredUnbalancedSearchTreeSuite extends AnyFunSuite {

  private val indexMapping = Seq(0 -> 16, 1 -> 3, 2 -> 7, 8 -> 4, 4 -> 90, 10 -> 21, 5 -> 0)

  private val stringMapping = Seq("main" -> 0, "apply" -> 23, "compare" -> 2, "toString" -> 4, "Sequence" -> 73)
  private val doubleMapping = Seq(3.3 -> "three-point-three", 4.5 -> "four-point-five", 7.0 -> "seven")
  private val reverseOrdering = Ordering.apply[Int].reverse

  test("the size of the empty tree should be 0") {
    val tree = UnbalancedSearchTree[Int, Int](Ordering[Int])
    assert(tree.size == 0)
  }

  test("the tree should also accept other key types than Int, e.g. String") {
    val tree = UnbalancedSearchTree[String, Int](Ordering[String])
    stringMapping.foreach((key, value) => tree.addOrUpdate(key, value))
    stringMapping.foreach((key, value) => assert(tree(key) == value))
  }

  test("the tree should also accept other key types than Int, e.g. Double") {
    val tree = UnbalancedSearchTree[Double, String](Ordering[Double])
    doubleMapping.foreach((key, value) => tree.addOrUpdate(key, value))
    doubleMapping.foreach((key, value) => assert(tree(key) == value))
  }

  test("trying to access a value in an empty tree should not work") {
    val tree = UnbalancedSearchTree(Ordering[Int])
    indexMapping.foreach((key, _) =>
      assertThrows[NoSuchElementException](tree(key))
    )
  }

  test("trying to call get on an empty tree should always result in None") {
    val tree = UnbalancedSearchTree[Int, Int](Ordering[Int])
    assert(indexMapping.forall((key, _) => tree.get(key) == None))
  }

  test("after inserting one key-value pair the size of the tree should increase by 1") {
    val tree = UnbalancedSearchTree[Int, Int](Ordering[Int])
    tree.addOrUpdate(0, 16)
    assert(tree.size == 1)
  }

  test("after inserting one key-value pair in different trees each size of the trees should increase by 1") {
    val tree1 = UnbalancedSearchTree[Int, Int](Ordering[Int])
    tree1.addOrUpdate(0, 16)
    assert(tree1.size == 1)
    val tree2 = UnbalancedSearchTree[Int, Int](Ordering[Int])
    tree2.addOrUpdate(0, 16)
    assert(tree2.size == 1)
  }

  test("a tree that contains a key-value pair should find it when we want to access it") {
    val tree = UnbalancedSearchTree[Int, Int](Ordering[Int])
    tree.addOrUpdate(0, 16)
    assert(tree(0) == 16)
  }

  test("a tree that contains a key-value pair should find it when we want to call get") {
    val tree = UnbalancedSearchTree[Int, Int](Ordering[Int])
    tree.addOrUpdate(0, 16)
    assert(tree.get(0) == Some(16))
  }

  test("clearing a tree with values should empty it so none of the previous keys are found") {
    val tree = UnbalancedSearchTree(indexMapping)(Ordering[Int])
    assert(tree.size == indexMapping.size)
    tree.clear()
    assert(tree.size == 0)
    indexMapping.forall((key, _) => tree.get(key).isEmpty)
  }

  test("after the tree has been cleared it should just behave like a freshly created empty tree") {
    val tree = UnbalancedSearchTree(indexMapping)(Ordering[Int])
    assert(tree.size == indexMapping.size)
    tree.clear()
    val updatedMapping = indexMapping.map((key, value) => (key, value * 2))
    updatedMapping.foreach((key, value) => tree.addOrUpdate(key, value))
    assert(tree.size == updatedMapping.size)
    assert(updatedMapping.forall((key, value) => tree(key) == value))
  }

  test("filling a tree with the index mappings should make it contain all of them") {
    val tree = UnbalancedSearchTree(indexMapping)(Ordering[Int])
    assert(tree.size == indexMapping.size)
    assert(indexMapping.forall((key, value) => tree(key) == value))
  }

  test("a tree should contain all index mappings even if the insertion order is not the same") {
    val tree = UnbalancedSearchTree[Int, Int](Ordering[Int])
    indexMapping.reverse.foreach((key, value) => tree.addOrUpdate(key, value))
    assert(indexMapping.forall((key, value) => tree(key) == value))
  }

  test("if a mapping already exists in the tree addOrUpdate should update the value associated with the key instead") {
    val tree = UnbalancedSearchTree(indexMapping)(reverseOrdering)
    val (key, _) = indexMapping(5)
    tree.addOrUpdate(key, 20)
    assert(tree.size == indexMapping.size)
    assert(tree(key) == 20)
  }

  test("updating a bunch of key-value pairs should work as intended") {
    val tree = UnbalancedSearchTree(indexMapping)(reverseOrdering)
    val updatedMappings = indexMapping.map((key, value) => (key, value * 2))
    updatedMappings.foreach((key, value) => tree.addOrUpdate(key, value))
    assert(tree.size == indexMapping.size)
    assert(updatedMappings.forall((key, value) => tree(key) == value))
  }
}
