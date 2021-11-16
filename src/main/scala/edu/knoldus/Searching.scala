package edu.knoldus

import scala.annotation.tailrec


class Searching {

  def binarySearch(array: Array[Int], elem: Int): Boolean = {
    @tailrec
    def bs(array: Array[Int], elem: Int, start: Int, end: Int): Int = {
      if (start > end) -1
      else {
        val mid = start + (end-start) / 2
        array(mid) match {
          case i if i == elem => mid
          case i if i > elem => bs(array, elem, start, mid - 1)
          case _ => bs(array, elem, mid + 1, end)

        } }
    }
    val found = bs(array, elem, start = 0, end = array.length - 1)
    if (found == (-1)) false
    else
      true }

  def linearSearch(array: Array[Int], elem: Int): Boolean = {
    @tailrec
    def lS(elem: Int, array: Array[Int], i: Int): Int = {
      if (array.head.equals(elem)) i
      else if (array.tail.isEmpty) -1
      else lS(elem, array.tail, i + 1)

    }
    val found=lS(elem, array,i = 0)
    if (found == (-1))
      false
    else
      true
  }






}
