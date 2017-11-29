package com.cibo.scalastan.models

import com.cibo.scalastan._

case class NaiveBayes(
  data: Seq[Map[Int, Set[Int]]],            // Mapping from group -> document ID (1 based) -> word IDs (1 based)
  topicPrior: Option[Seq[Double]] = None,   // Topic prior
  wordPrior: Option[Seq[Double]] = None     // Word prior
) extends ScalaStan {

  // Naive Bayes Classification
  // from "Stan Modeling Language: User's Guide and Reference Manual" version 2.16.0.

  private lazy val documentWords: Seq[(Int, Int)] = data.flatMap { group =>
    group.toSeq.flatMap { case (d, ws) =>
      ws.toSeq.map(w => d -> w)
    }
  }
  private lazy val documentTopics: Seq[Int] = data.zipWithIndex.flatMap { case (mapping, group) =>
    mapping.keys.map(doc => doc -> (group + 1))
  }.sorted.map(_._2)
  private lazy val words = documentWords.map(_._2)
  private lazy val documents = documentWords.map(_._1)
  private lazy val topicCount: Int = documentTopics.max
  private lazy val wordCount: Int = words.max
  private lazy val documentCount: Int = documents.max

  require(documentWords.forall(_._1 > 0), "Document IDs must be > 0")
  require(documentWords.forall(_._2 > 0), "Word IDs must be > 0")
  require(documents.toSet.size == documentCount, "Document IDs must start from 1 and be contiguous")
  require(words.toSet.size == wordCount, "Word IDs must start from 1 and be contiguous")

  private val k = data(int(lower = 1))                  // Number of topics
  private val v = data(int(lower = 1))                  // Number of words
  private val m = data(int(lower = 0))                  // Number of documents
  private val n = data(int(lower = 0))                  // Total word instances
  private val z = data(int(lower = 1, upper = k)(m))    // Topic for document m
  private val w = data(int(lower = 1, upper = v)(n))    // Word n
  private val doc = data(int(lower = 1, upper = m)(n))  // Document ID for word n
  private val alpha = data(vector(k, lower = 0))        // Topic prior
  private val beta = data(vector(v, lower = 0))         // Word prior

  val theta: StanParameterDeclaration[StanVector] = parameter(simplex(k))             // Topic prevalence
  val phi: StanParameterDeclaration[StanArray[StanVector]] = parameter(simplex(v)(k))  // Word distribution for topic k

  private val model = new Model {
    theta ~ stan.dirichlet(alpha)
    for (i <- range(1, k)) {
      phi(i) ~ stan.dirichlet(beta)
    }
    for (i <- range(1, m)) {
      z(i) ~ stan.categorical(theta)
    }
    for (i <- range(1, n)) {
      w(i) ~ stan.categorical(phi(z(doc(i))))
    }
  }

  private def defaultTopicPrior: Seq[Double] = Seq.fill[Double](topicCount)(1.0 / topicCount)

  private def defaultWordPrior: Seq[Double] = Seq.fill[Double](wordCount)(1.0 / wordCount)

  def compile[M <: CompiledModel](implicit runner: StanRunner[M]): CompiledModel = model.compile
    .withData(k, topicCount)
    .withData(v, wordCount)
    .withData(m, documentCount)
    .withData(z, documentTopics)
    .withData(w, words)
    .withData(doc, documents)
    .withData(alpha, topicPrior.getOrElse(defaultTopicPrior))
    .withData(beta, wordPrior.getOrElse(defaultWordPrior))

  def scores(ws: Set[Int], results: StanResults): Seq[Double] = {
    val bestTheta = results.best(theta)
    val bestPhi = results.best(phi)
    bestPhi.zip(bestTheta).map { case (ps, t) =>
      t * ps.zipWithIndex.filter(x => ws.contains(x._2 + 1)).map(_._1).product
    }
  }

  def classify(ws: Set[Int], results: StanResults): Int = {
    scores(ws, results).zipWithIndex.maxBy(_._1)._2 + 1
  }
}
