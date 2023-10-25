package io.lambdaworks.workshop.purity
import java.util.{ArrayList => JList}

import org.joda.time.DateTime

object EitherPureOrNot {

  final case class Todo(id: Int, title: String)

  private var todoList = Seq(Todo(1, "Make a coffee!"), Todo(2, "Clean the house!"))
  private val builder  = new StringBuilder()

  // Either pure or not?

  //Pure
  def addTodo(todo: Todo): Unit = todoList :+= todo

  //Not pure, doesn't always return the same value
  def currentDate: DateTime = DateTime.now

  //Pure
  def evenNumbers(lowerBound: Int, upperBound: Int): Seq[Int] = {
    var result = Seq[Int]()
    for (index <- lowerBound to upperBound) {
      if (0 == index % 2) result :+= index
    }

    result
  }

  //Not pure, has println
  def firstElement(todoList: List[Todo]): Todo = {
    val head = todoList.head
    println(head)

    head
  }

  //Not pure, uses object which is outside of a function scope
  def fullName(firstName: String, lastName: String): String = {
    builder.append(firstName)
    builder.append(lastName)
    builder.mkString(" ")
  }

  //Not pure, mutates a variable that was passed in
  def square4j(numbers: JList[Int]): JList[Int] = {
    for (index <- 0 until numbers.size()) {
      val number = numbers.get(index)
      numbers.set(index, number * number)
    }

    numbers
  }

  //Pure
  def square4s(numbers: Seq[Int]): Seq[Int] = {
    var result = numbers
    for (index <- numbers.indices) {
      val number = numbers(index)
      result = numbers.updated(index, number * number)
    }

    result
  }

}
