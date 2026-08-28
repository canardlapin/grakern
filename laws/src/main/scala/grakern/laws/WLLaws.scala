package grakern.laws

import grakern.core.*
import grakern.standard.wl.*
import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

/** Discipline laws for WL refinement and WL kernel programs, evaluated portably through
  * [[WLReference]].
  *
  * Scope of the numerical claims: PSD and the [0, 1] normalization bounds are stated for the
  * subtree and optimal-assignment families and their non-negative combinations (`+`, `weighted`),
  * which are histogram dot products and hierarchy intersections respectively — both PSD by
  * construction. They are not claimed for arbitrary user programs.
  */
object WLLaws extends Laws:
  import GraphGens.given

  type Ref = WLRefinement[String, String, String, String]
  type Program = Ref => WLKernel[String, String, String, String]

  def refinement(rounds: Int): Ref =
    WLRefinement.create(
      rounds,
      Projection.of[String, String](identity),
      Projection.of[String, String](identity),
      WLCodecs[String, String](None, None)
    ) match
      case Right(r)    => r
      case Left(error) => throw new IllegalStateException(s"invalid refinement: $error")

  private def traced(graphs: Vector[LabelledGraph], rounds: Int) =
    val r = refinement(rounds)
    val samples: Vector[LabelledNeighbourhood[Int, String, String]] =
      graphs.zipWithIndex.map { case (g, i) => g.sample(s"g$i") }
    (r, WLReference.trace(samples, r))

  private val rounds: Gen[Int] = Gen.choose(0, 3)

  /** Kernel laws for one WL program family over generated graph sets. */
  def kernel(name: String, program: Program, directed: Boolean): RuleSet =
    new DefaultRuleSet(
      name = s"wl-kernel[$name,${if directed then "directed" else "undirected"}]",
      parent = None,
      "symmetry" -> forAll(GraphGens.graphSet(directed), rounds) { (graphs, n) =>
        val (r, ref) = traced(graphs, n)
        val k = program(r)
        val all = graphs.indices
        all.forall(i =>
          all.forall(j => Tolerance.close(ref.evaluate(k)(i, j), ref.evaluate(k)(j, i), 1e-9))
        )
      },
      "gram is positive semidefinite" -> forAll(GraphGens.graphSet(directed), rounds) {
        (graphs, n) =>
          val (r, ref) = traced(graphs, n)
          val gram = ref.gram(program(r))
          val min = SymmetricEigen.minEigenvalue(gram)
          val scale = math.max(1.0, gram.map(_.map(math.abs).max).max)
          Prop(min >= -Tolerance.psd * scale) :| s"min eigenvalue $min"
      },
      "self kernel is non-negative" -> forAll(GraphGens.graphSet(directed), rounds) { (graphs, n) =>
        val (r, ref) = traced(graphs, n)
        val k = program(r)
        graphs.indices.forall(i => ref.evaluate(k)(i, i) >= 0.0)
      },
      "identical graphs have identical rows" -> forAll(GraphGens.graph(directed), rounds) {
        (g, n) =>
          val (r, ref) = traced(Vector(g, g), n)
          val k = program(r)
          Tolerance.close(ref.evaluate(k)(0, 1), ref.evaluate(k)(0, 0), 1e-9) &&
          Tolerance.close(ref.evaluate(k)(0, 0), ref.evaluate(k)(1, 1), 1e-9)
      }
    )

  /** `normalized` bounds for a WL program family. */
  def normalized(name: String, program: Program, directed: Boolean): RuleSet =
    new DefaultRuleSet(
      name = s"wl-normalized[$name,${if directed then "directed" else "undirected"}]",
      parent = None,
      "bounded in [0, 1]" -> forAll(GraphGens.graphSet(directed), rounds) { (graphs, n) =>
        val (r, ref) = traced(graphs, n)
        val k = program(r).normalized
        graphs.indices.forall(i =>
          graphs.indices.forall { j =>
            val v = ref.evaluate(k)(i, j)
            v >= -Tolerance.bounds && v <= 1.0 + Tolerance.bounds
          }
        )
      },
      "unit diagonal where the base diagonal is positive" -> forAll(
        GraphGens.graph(directed),
        rounds
      ) { (g, n) =>
        val (r, ref) = traced(Vector(g), n)
        val base = ref.evaluate(program(r))(0, 0)
        val v = ref.evaluate(program(r).normalized)(0, 0)
        if base > 0.0 then Tolerance.close(v, 1.0, Tolerance.bounds) else v == 0.0
      }
    )

  /** Algebra of WL programs: `+` associative/commutative, `weighted` homogeneous. */
  def algebra(directed: Boolean): RuleSet =
    new DefaultRuleSet(
      name = s"wl-algebra[${if directed then "directed" else "undirected"}]",
      parent = None,
      "sum is associative and commutative" -> forAll(GraphGens.graphSet(directed), rounds) {
        (graphs, n) =>
          val (r, ref) = traced(graphs, n)
          val (x, y, z) = (r.subtree, r.optimalAssignment, r.subtree.normalized)
          graphs.indices.forall(i =>
            graphs.indices.forall { j =>
              Tolerance
                .close(ref.evaluate((x + y) + z)(i, j), ref.evaluate(x + (y + z))(i, j), 1e-9) &&
              Tolerance.close(ref.evaluate(x + y)(i, j), ref.evaluate(y + x)(i, j), 1e-9)
            }
          )
      },
      "weighted is homogeneous" -> forAll(GraphGens.graphSet(directed), rounds, KernelLaws.weight) {
        (graphs, n, w) =>
          val (r, ref) = traced(graphs, n)
          val x = r.subtree + r.optimalAssignment
          graphs.indices.forall(i =>
            graphs.indices.forall(j =>
              Tolerance
                .close(ref.evaluate(x.weighted(w))(i, j), w.toDouble * ref.evaluate(x)(i, j), 1e-9)
            )
          )
      },
      "round weights must match rounds + 1" -> forAll(rounds) { n =>
        val r = refinement(n)
        r.subtree(Vector.fill(n + 2)(NonNegativeFinite.one)).isLeft &&
        r.subtree(Vector.fill(n + 1)(NonNegativeFinite.one)).isRight
      }
    )

  /** Refinement laws: determinism, alpha-invariance, monotone refinement, and rounds = 0. */
  def refinementLaws(directed: Boolean): RuleSet =
    new DefaultRuleSet(
      name = s"wl-refinement[${if directed then "directed" else "undirected"}]",
      parent = None,
      "determinism: two traces agree on every canonical partition" -> forAll(
        GraphGens.graph(directed),
        rounds
      ) { (g, n) =>
        val (_, first) = traced(Vector(g), n)
        val (_, second) = traced(Vector(g), n)
        (0 to n).forall(r => first.canonicalPartition(0, r) == second.canonicalPartition(0, r))
      },
      "alpha-invariance and enumeration invariance of partitions and kernels" -> forAll(
        GraphGens.relabelling(GraphGens.graph(directed)),
        rounds
      ) { (rel, n) =>
        val (r, ref) = traced(Vector(rel.graph, rel.relabelled), n)
        val p = rel.vertexPermutation
        val partitions =
          (0 to n).forall { round =>
            val original = ref.colours(0)(round)
            val renamed = ref.colours(1)(round)
            original.indices.forall(v => original(v) == renamed(p(v)))
          }
        val k = r.subtree + r.optimalAssignment
        partitions &&
        ref.evaluate(k)(0, 0) == ref.evaluate(k)(1, 1) &&
        ref.evaluate(k)(0, 1) == ref.evaluate(k)(0, 0)
      },
      "monotone: the partition at round r + 1 refines round r" -> forAll(
        GraphGens.graph(directed),
        Gen.choose(1, 3)
      ) { (g, n) =>
        val (_, ref) = traced(Vector(g), n)
        (1 to n).forall { round =>
          val coarse = ref.colours(0)(round - 1)
          val fine = ref.colours(0)(round)
          coarse.indices.forall(u =>
            coarse.indices.forall(v => fine(u) != fine(v) || coarse(u) == coarse(v))
          )
        }
      },
      "rounds = 0 is the label histogram kernel" -> forAll(GraphGens.graphSet(directed)) { graphs =>
        val (r, ref) = traced(graphs, 0)
        val k = r.subtree
        graphs.indices.forall(i =>
          graphs.indices.forall { j =>
            val a = graphs(i).labels.groupMapReduce(identity)(_ => 1)(_ + _)
            val b = graphs(j).labels.groupMapReduce(identity)(_ => 1)(_ + _)
            val expected = a.map { case (l, c) => c.toDouble * b.getOrElse(l, 0).toDouble }.sum
            Tolerance.close(ref.evaluate(k)(i, j), expected, 1e-9)
          }
        )
      }
    )

  /** Directed-graph laws G1 (direction changes features) and G2 (invariance). */
  val directedLaws: RuleSet =
    new DefaultRuleSet(
      name = "wl-directed",
      parent = None,
      "G2: alpha-renaming and arc reordering leave every kernel value bit-identical" -> forAll(
        GraphGens.relabelling(GraphGens.directed),
        GraphGens.directed,
        Gen.choose(1, 3)
      ) { (rel, other, n) =>
        val (r, ref) = traced(Vector(rel.graph, rel.relabelled, other), n)
        val k = r.subtree + r.optimalAssignment
        ref.evaluate(k)(0, 2) == ref.evaluate(k)(1, 2) &&
        ref.evaluate(k)(0, 0) == ref.evaluate(k)(1, 1) &&
        ref.evaluate(k.normalized)(0, 1) == 1.0
      },
      "G1: reversing a non-automorphic arc changes the features from round 1" -> forAll(
        GraphGens.arcReversal,
        Gen.choose(1, 3)
      ) { (reversal, n) =>
        val (r, ref) = traced(Vector(reversal.graph, reversal.reversed), n)
        val normalizedValue = ref.evaluate(r.subtree.normalized)(0, 1)
        Prop(normalizedValue < 1.0 - 1e-9) :| s"normalized kernel $normalizedValue" &&
        Prop(ref.histograms(0)(1) != ref.histograms(1)(1)) :| "round-1 histograms differ"
      },
      "undirected orientation is symmetric: an undirected edge is the same seen from both ends" ->
        forAll(GraphGens.undirected, Gen.choose(1, 3)) { (g, n) =>
          val sample = g.sample("u")
          val symmetric =
            sample.vertices.forall(v =>
              sample
                .incidences(v)
                .forall(inc =>
                  inc.orientation == Orientation.Undirected &&
                    sample
                      .incidences(inc.other)
                      .exists(back =>
                        back.other == v && back.value == inc.value &&
                          back.orientation == Orientation.Undirected
                      )
                )
            )
          val (_, ref) = traced(Vector(g), n)
          symmetric && ref.graphCount == 1
        }
    )

  /** Hierarchy / optimal-assignment laws: OA is a valid (PSD) kernel and threshold features are
    * monotone in the threshold; OA is monotone under bag growth.
    */
  def hierarchyLaws(directed: Boolean): RuleSet =
    new DefaultRuleSet(
      name = s"wl-hierarchy[${if directed then "directed" else "undirected"}]",
      parent = None,
      "optimal assignment gram is positive semidefinite" -> forAll(
        GraphGens.graphSet(directed),
        rounds
      ) { (graphs, n) =>
        val (r, ref) = traced(graphs, n)
        val gram = ref.gram(r.optimalAssignment)
        val min = SymmetricEigen.minEigenvalue(gram)
        val scale = math.max(1.0, gram.map(_.map(math.abs).max).max)
        Prop(min >= -Tolerance.psd * scale) :| s"min eigenvalue $min"
      },
      "optimal assignment is bounded by the smaller self kernel" -> forAll(
        GraphGens.graphSet(directed),
        rounds
      ) { (graphs, n) =>
        val (r, ref) = traced(graphs, n)
        val k = r.optimalAssignment
        graphs.indices.forall(i =>
          graphs.indices.forall(j =>
            ref.evaluate(k)(i, j) <= math.min(ref.evaluate(k)(i, i), ref.evaluate(k)(j, j)) + 1e-9
          )
        )
      },
      "threshold features are monotone in the threshold" -> forAll(thresholdHierarchy) {
        case (hierarchy, bag, _) =>
          val features = hierarchy.thresholdFeatures(bag).map(_._1)
          features.forall(f =>
            f.multiplicityThreshold == 1 ||
              features.contains(HierarchyFeature(f.node, f.multiplicityThreshold - 1))
          )
      },
      "optimal assignment is monotone under bag growth" -> forAll(thresholdHierarchy) {
        case (hierarchy, bag, leafItems) =>
          val grown = FiniteBag(bag.items :+ leafItems.head)
          hierarchy.optimalAssignment(grown, bag) >= hierarchy.optimalAssignment(bag, bag) - 1e-9
      }
    )

  private val thresholdHierarchy
      : Gen[(Hierarchy[Int, WLReference.Node], FiniteBag[Int], Vector[Int])] =
    for
      depth <- Gen.choose(1, 3)
      width <- Gen.choose(1, 3)
      bagSize <- Gen.choose(0, 6)
      picks <- Gen.listOfN(bagSize, Gen.choose(0, depth * width - 1))
    yield
      import WLReference.Node
      val colours = Vector.tabulate(depth * width)(identity)
      val nodes = cats.data.NonEmptyVector(Node.Root, colours.map(Node.Colour(_)))
      val parent: Node => Option[Node] =
        case Node.Root      => None
        case Node.Colour(i) =>
          val level = i / width
          if level == 0 then Some(Node.Root) else Some(Node.Colour(i - width))
      val height: Node => NonNegativeFinite =
        case Node.Root      => NonNegativeFinite.zero
        case Node.Colour(i) =>
          NonNegativeFinite.from((i / width).toDouble + 1.0).getOrElse(NonNegativeFinite.zero)
      val leaves = colours.filter(_ / width == depth - 1).map(i => i -> Node.Colour(i))
      val hierarchy =
        Hierarchy.build[Node, Int](nodes, Node.Root, parent, height, leaves)(using
          WLReference.nodeHash,
          WLReference.intEq
        ) match
          case Right(h)    => h
          case Left(error) => throw new IllegalStateException(s"hierarchy: $error")
      val leafItems = leaves.map(_._1)
      (
        hierarchy,
        FiniteBag(picks.toVector.map(p => leafItems(p % leafItems.size))),
        leafItems
      )
