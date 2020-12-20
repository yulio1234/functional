package com.yuli.functional.chapter2

/**
 * 描述:函数式例子
 *
 * @author yuli
 * @date 2020/7/4 
 */
object MyModule {
  /**
   * 去绝对值
   *
   * @param n
   * @return
   */
  def abs(n: Int): Int = {
    if (n < 0) -n else n
  }

  /**
   * 阶乘，将函数定义在方法内部
   *
   * @param n
   * @return
   */
  def factorial(n: Int): Int = {
    def loop(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else loop(n - 1, n * acc)
    }

    loop(n, 1)
  }


  /**
   * 练习，找出第n个fib数
   *
   * @param n
   * @return
   */
  def fib(n: Int): Int = {
    def loop(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else loop(n - 1, acc + 1)
    }

    loop(n, 0)
  }

  /**
   * 格式化绝对值
   *
   * @param x
   * @return
   */
  private def formatAbs(x: Int): String = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  /**
   * 格式化阶乘
   *
   * @param n
   */
  private def formatFactorial(n: Int): String = {
    val msg = "The factorial of %d is %d"
    msg.format(n, factorial(n))
  }

  /**
   * 高阶函数，将函数作为参数，泛化格式化方法
   *
   * @param name
   * @param n
   * @param f
   */
  def formatResult(name: String, n: Int, f: Int => Int): String = {
    val msg = "The %s of %d is %d";
    msg.format(name, n, f(n))
  }

  /**
   * 多态函数，通过范型匹配任意类型
   *
   * @param array
   * @param p
   * @tparam A
   * @return
   */
  def findFirst[A](array: Array[A], p: A => Boolean): Int = {
    def loop(n: Int): Int = {//使用递归来查找，没有副作用
      if (n >= array.length) -1
      else if (p(array(n))) n
      else loop(n + 1)
    }
    loop(0)
  }

  /**
   * 匹配是否按照特定的顺序排序
   * @param array
   * @param ordered
   * @tparam A
   * @return
   */
  def isSorted[A](array:Array[A],ordered:(A,A) => Boolean) :Boolean = {
    def loop(n:Int) :Boolean = {
      if( n == array.length -1 ) true
      else if(!ordered(array(n),array(n+1))) false
      else loop(n + 1)
    }
    loop(0)
  }

  /**
   * 类型多态的部分应用函数，返回一个带有一个参数的函数，b是需要后续方法定义的类型
   * @param a
   * @param f
   * @tparam A
   * @tparam B
   * @tparam C
   * @return
   */
  def partial[A,B,C](a:A,f:(A,B)=> C):B => C = (b:B)=>f(a,b)

  /**
   * 练习2.3，柯里化例子，返回部分应用的函数，在返回的函数中需要定义a和b，在应用函数f。函数是右结合的，可以省略（B=>C）
   * @param f
   * @tparam A
   * @tparam B
   * @tparam C
   * @return
   */
  def curry[A,B,C](f:(A,B) => C):A=>B=>C=(a:A)=>(b:B)=>f(a,b)

  /**
   * 练习2.4 反柯里化，再返回的函数中需要定义A、B类型的对偶
   * @param f
   * @tparam A
   * @tparam B
   * @tparam C
   * @return
   */
  def uncurry[A,B,C](f:A=>B=>C):(A,B)=>C = (a:A,b:B)=>f(a)(b)

  /**
   * 组合函数
   * @param f
   * @param g
   * @tparam A
   * @tparam B
   * @tparam C
   * @return
   */
  def compose[A,B,C](f:B=>C,g:A=>B):A=>C = (a:A) =>f(g(a))


  def add(i:Int,x:Int): Int ={
    if(i == 0) x
    else add(i-1,x + 1)
  }
  def main(args: Array[String]): Unit = {

    println("add"+add(5, 1))

    //基本的函数
    println(formatAbs(-42))
    println(formatFactorial(3))

    //使用高等函数经过泛化的方法
    println(formatResult("absolute", -42, abs))
    println(formatResult("factorial", 3, factorial))

    //多态函数,使用函数字面量
    println(findFirst(Array(1, 3, 5, 7, 9), (x: Int) => x == 9))

    //按照特定顺序排序
    println(isSorted(Array(1, 2, 3, 4, 5, 6), (x: Int, y: Int) => y == x + 1))
  }
}
