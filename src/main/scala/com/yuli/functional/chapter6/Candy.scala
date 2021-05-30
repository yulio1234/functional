package com.yuli.functional.chapter6

import com.yuli.functional.chapter6.State.{modify, sequence, get}


object Candy {
  def update: Input => Machine => Machine = (i: Input) => (s: Machine) => (i, s) match {
    case (_, Machine(_, 0, _)) => s
    case (Coin, Machine(false, _, _)) => s
    case (Turn, Machine(true, _, _)) => s
    case (Coin, Machine(true, candies, coins)) => Machine(false, candies, coins + 1)
    case (Turn, Machine(false, candies, coins)) => Machine(true, candies - 1, coins)
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs.map(input => modify {
      update(input)
    }))
    s <- get
  } yield (s.coins, s.candies)

  def main(args: Array[String]): Unit = {
    val tuple = Candy.simulateMachine(List(Coin, Turn)).run(Machine(true, 3, 0))
    println(tuple)
  }
}
