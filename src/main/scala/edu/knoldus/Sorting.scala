package edu.knoldus

class Sorting {

  def insertionSort(array: Array[Int]): Array[Int] = {

    def isort(list: List[Int]): List[Int] = {
      if (list.isEmpty) Nil
      else insert(list.head, isort(list.tail))
    }

    def insert(x: Int, xs: List[Int]): List[Int] = {
      if (xs.isEmpty || x <= xs.head) x :: xs
      else xs.head :: insert(x, xs.tail)
    }

    val list: List[Int] = array.toList
    val sortedList = isort(list)
    sortedList.toArray
  }

  def selectionSort(array: Array[Int]): Array[Int] = {
    for(i<-0 until array.length-1){
      for(j<-i + 1 until array.length){
        if (array(i)>array(j)){
          val temp=array(j)
          array(j)=array(i)
          array(i)=temp
        }
      }

    }

    array
  }

  def bubbleSort(array: Array[Int]): Array[Int] = {
    for (i <- 0 until array.length - 1) {
      for (j <- 0 until array.length - i - 1) {
        if (array(j) > array(j + 1)) {
          val temp = array(j)
          array(j) = array(j + 1)
          array(j + 1) = temp
        }
      }
    }
    array
  }
}