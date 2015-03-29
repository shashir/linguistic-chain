/**
 * Given a word and a dictionary, produces a sequence of words such that each word has
 * one less character than the predecessor and also belongs in the dictionary. The sequence
 * is expected to be the longest such chain.
 *
 * For example, given "starting" we get: starting => stating => statin => satin => sati
 * => sat => at => a
 * Assuming the dictionary contains all of those words.
 *
 * Dictionaries are expected to have one word per line.
 */
object LingChain extends App {
  // Validate parameters.
  require(
    args.size == 2,
    """Please provide two positional arguments, the path to the dictionary file and the string
      |input. E.g. scala LingChain ./dictionary.txt starting""".stripMargin
  )

  // Collect parameters.
  val dictionaryFile: String = args(0)
  val input: String = args(1)

  // Read dictionary file.
  val source = scala.io.Source.fromFile(dictionaryFile)
  val dictionary = source.getLines.toSet
  source.close()

  // If the input word is not in the dictionary, warn and proceed.
  if (!dictionary.contains(input)) {
    println("Input word is not in the dictionary. Continuing with substrings.")
  }

  // Build and display chain.
  buildChain(input).foreach { seq: Seq[String] => println(seq.mkString(" => ")) }

  // Helpers ---------------------------------------------------------------------------------------
  /**
   * For a given string, produces all chains of substrings where each successor is in the
   * dictionary and has one less character than its predecessor.
   *
   * @param input string to decompose into chains of substrings.
   * @return set of all chains of substrings where each successor is in the dictionary and has one
   *         less character than its predecessor
   */
  def buildChain(input: String): Set[Seq[String]] = {
    buildChain(Set(Node(None, input))).map { node: Node => Node.getPathFromRoot(node) }
  }

  /**
   * For a given set of nodes, where each node has a string value, recursively compute each child
   * node whose string value exists in the dictionary and has one less character than the value
   * of its parent. In this way we are building a tree of nodes.
   *
   * @param chains set of nodes, where each node has a string value.
   * @return set of nodes, where each node has a string value which is in the dictionary and
   *         contains one less character than a string value of the input node.
   */
  def buildChain(chains: Set[Node]): Set[Node] = {
    val candidateChains: Set[Node] = chains.flatMap { chain: Node =>
      val value: String = chain.value
      (0 until value.length).toSet
          .map { pos: Int => value.substring(0, pos) + value.substring(pos + 1) }
          .filter { dictionary.contains(_) }
          .map { Node(Some(chain), _) }
    }
    if (candidateChains.size == 0) {
      chains
    } else {
      buildChain(candidateChains)
    }
  }

  /**
   * A node record for building simple trees.
   *
   * @param parent of the node. Optional.
   * @param value string associated with the node.
   */
  final case class Node(
    parent: Option[Node],
    value: String
  )

  /**
   * Companion to Node class.
   */
  object Node {
    /**
     * A method which takes a leaf node and produces a path of strings associated with each node
     * from the root to the leaf.
     *
     * @param node current node.
     * @param path path of strings discerned so far.
     * @return path discerned so far appended to rest of the path to the root.
     */
    def getPathFromRoot(node: Node, path: Seq[String] = Seq()): Seq[String] = {
      val newPath: Seq[String] = path.+:(node.value)
      if (node.parent.isDefined) {
        getPathFromRoot(node.parent.get, newPath)
      } else {
        newPath
      }
    }
  }
}

