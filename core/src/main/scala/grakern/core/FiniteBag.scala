package grakern.core

import cats.kernel.Eq

/** A finite bag that retains occurrence order and exact integer multiplicity.
  *
  * Order is not part of optimal-assignment semantics. It is retained so reference evaluators and
  * provenance can refer to concrete occurrences without inventing another identity.
  */
final case class FiniteBag[+A](items: Vector[A]):
  def size: Int = items.size
  def isEmpty: Boolean = items.isEmpty
  def nonEmpty: Boolean = items.nonEmpty
  def iterator: Iterator[A] = items.iterator

  def map[B](f: A => B): FiniteBag[B] =
    FiniteBag(items.map(f))

  def ++[B >: A](that: FiniteBag[B]): FiniteBag[B] =
    FiniteBag(items ++ that.items)

  def multiplicityOf[B >: A](value: B)(using equality: Eq[B]): Int =
    items.count(equality.eqv(_, value))

object FiniteBag:
  def empty[A]: FiniteBag[A] = FiniteBag(Vector.empty)

  def one[A](item: A): FiniteBag[A] = FiniteBag(Vector(item))

  def from[A](items: IterableOnce[A]): FiniteBag[A] =
    FiniteBag(items.iterator.toVector)
