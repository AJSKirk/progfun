package week4

/**
  * Created by ankirk on 26/07/2016.
  */

abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat = {
    if (that.isZero) this
    else this.successor + that.predecessor
  }
  def - (that: Nat): Nat = {
    if (that.isZero) this
    else this.predecessor - that.predecessor
  }
}

object Zero extends Nat {
  def isZero = true
  def predecessor = throw new Error("0.predecessor")
}

class Succ(n: Nat) extends Nat {
  def isZero = false
  def predecessor = n
}


