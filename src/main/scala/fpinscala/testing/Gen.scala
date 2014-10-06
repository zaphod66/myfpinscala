package fpinscala.testing

trait Prop {
  def check: Boolean
  
  // exercise 8.3
  def &&(p: Prop) = this.check && p.check
}
