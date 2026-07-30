package grakern.core

/** Session-local identity and durable-description contract shared by refinement programs.
  *
  * The refined trace remains recipe-specific: this contract intentionally does not impose a
  * universal colour or storage representation.
  */
trait Refinement:
  def planNodeId: PlanNodeId
  def fingerprintability: Fingerprintability
