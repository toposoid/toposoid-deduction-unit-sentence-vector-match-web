/*
 * Copyright (C) 2025  Linked Ideal LLC.[https://linked-ideal.com/]
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package controllers

import akka.util.Timeout
import com.ideal.linked.common.DeploymentConverter.conf
import com.ideal.linked.toposoid.common.{CLAIM, PREMISE, PROPOSITION_ID, TRANSVERSAL_STATE, ToposoidUtils, TransversalState, UNSPECIFIED}
import com.ideal.linked.toposoid.knowledgebase.featurevector.model.{FeatureVectorId, FeatureVectorIdentifier}
import com.ideal.linked.toposoid.knowledgebase.regist.model.{Knowledge, KnowledgeSentenceSet, PropositionRelation}
import com.ideal.linked.toposoid.protocol.model.base.AnalyzedSentenceObjects
import com.ideal.linked.toposoid.protocol.model.parser.{InputSentence, InputSentenceForParser, KnowledgeForParser, KnowledgeSentenceSetForParser}
import com.ideal.linked.toposoid.test.utils.TestUtils
import com.ideal.linked.toposoid.vectorizer.FeatureVectorizer
import controllers.TestUtilsEx.{deleteFeatureVector, deleteNeo4JAllData, registerSingleClaim}
import io.jvm.uuid.UUID
import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll}
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Play.materializer
import play.api.http.Status.OK
import play.api.libs.json.Json
import play.api.test.Helpers.{POST, contentType, status, _}
import play.api.test.{FakeRequest, _}

import scala.concurrent.duration.DurationInt

class HomeControllerSpecJapanese4 extends PlaySpec with BeforeAndAfter with BeforeAndAfterAll with GuiceOneAppPerSuite  with DefaultAwaitTimeout with Injecting {

  val transversalState:TransversalState = TransversalState(userId="test-user", username="guest", roleId=0, csrfToken = "")
  val transversalStateJson:String = Json.toJson(transversalState).toString()

  before {
    deleteNeo4JAllData(transversalState)
    Thread.sleep(5000)
  }

  override def beforeAll(): Unit = {
    ToposoidUtils.callComponent("{}", conf.getString("TOPOSOID_SENTENCE_VECTORDB_ACCESSOR_HOST"), conf.getString("TOPOSOID_SENTENCE_VECTORDB_ACCESSOR_PORT"), "createSchema", transversalState)
    deleteNeo4JAllData(transversalState)
  }

  override def afterAll(): Unit = {
    deleteNeo4JAllData(transversalState)
  }

  override implicit def defaultAwaitTimeout: Timeout = 600.seconds
  val controller: HomeController = inject[HomeController]

  val sentenceA = "自然界の法則がすべての慣性系で同じように成り立っている。"
  val sentenceB = "どの慣性系から見ても光の速さは一定である。"
  val sentenceC = "運動する物体の速さの上限は光の速さである。"
  val sentenceD = "特殊相対性理論では運動する物体の時間は進みかたが遅くなる。"

  val paraphraseA = "自然界の物理法則は例外なくどの慣性系でも成立する。"
  val paraphraseB = "見ている慣性系によらず光速は不変である。"
  val paraphraseC = "物体の運動する速さは光の速さを超えない。"
  val paraphraseD = "特殊相対性理論において物体は運動することにより時間がゆっくり進む。"


  "The specification31" should {
    "returns an appropriate response" in {
      val propositionId1 = UUID.random.toString
      val propositionId2 = UUID.random.toString
      val sentenceId1 = UUID.random.toString
      val sentenceId2 = UUID.random.toString
      val knowledge1 = Knowledge(sentenceA,"ja_JP", "{}", false)
      val knowledge2 = Knowledge(sentenceB,"ja_JP", "{}", false)
      //val knowledge3 = Knowledge(sentenceC,"ja_JP", "{}", false)
      //val knowledge4 = Knowledge(sentenceD,"ja_JP", "{}", false)

      val paraphrase1 = Knowledge(paraphraseA,"ja_JP", "{}", false)
      val paraphrase2 = Knowledge(paraphraseB,"ja_JP", "{}", false)
      val paraphrase3 = Knowledge(paraphraseC,"ja_JP", "{}", false)
      val paraphrase4 = Knowledge(paraphraseD,"ja_JP", "{}", false)

      registerSingleClaim(KnowledgeForParser(propositionId1, sentenceId1, knowledge1), transversalState)
      registerSingleClaim(KnowledgeForParser(propositionId2, sentenceId2, knowledge2), transversalState)
      val propositionIdForInference = UUID.random.toString
      val premiseKnowledge = List(KnowledgeForParser(propositionIdForInference, UUID.random.toString, paraphrase1), KnowledgeForParser(propositionIdForInference, UUID.random.toString, paraphrase2))
      val claimKnowledge = List(KnowledgeForParser(propositionIdForInference, UUID.random.toString, paraphrase3), KnowledgeForParser(propositionIdForInference, UUID.random.toString, paraphrase4))
      val inputSentence = Json.toJson(InputSentenceForParser(premiseKnowledge, claimKnowledge)).toString()

      val json = ToposoidUtils.callComponent(inputSentence, conf.getString("TOPOSOID_SENTENCE_PARSER_JP_WEB_HOST"), conf.getString("TOPOSOID_SENTENCE_PARSER_JP_WEB_PORT"), "analyze", transversalState)
      val fr = FakeRequest(POST, "/execute")
        .withHeaders("Content-type" -> "application/json", TRANSVERSAL_STATE.str -> transversalStateJson)
        .withJsonBody(Json.parse(json))
      val result = call(controller.execute(), fr)
      status(result) mustBe OK
      contentType(result) mustBe Some("application/json")
      val jsonResult: String = contentAsJson(result).toString()
      val analyzedSentenceObjects: AnalyzedSentenceObjects = Json.parse(jsonResult).as[AnalyzedSentenceObjects]
      assert(analyzedSentenceObjects.analyzedSentenceObjects.filter(x => x.knowledgeBaseSemiGlobalNode.sentenceType.equals(PREMISE.index) && x.deductionResult.status).size == 2)
      assert(analyzedSentenceObjects.analyzedSentenceObjects.filter(x => x.knowledgeBaseSemiGlobalNode.sentenceType.equals(CLAIM.index) && x.deductionResult.status).size == 0)
      assert(analyzedSentenceObjects.analyzedSentenceObjects.filter(x => x.knowledgeBaseSemiGlobalNode.sentenceType.equals(CLAIM.index) && x.deductionResult.havePremiseInGivenProposition).size == 0)
      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId1, featureId = sentenceId1, sentenceType = CLAIM.index, lang = "ja_JP", PROPOSITION_ID.index, UNSPECIFIED.index), transversalState)
      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId2, featureId = sentenceId2, sentenceType = CLAIM.index, lang = "ja_JP", PROPOSITION_ID.index, UNSPECIFIED.index), transversalState)
    }
  }

  "The specification32" should {
    "returns an appropriate response" in {
      val propositionId1 = UUID.random.toString
      val propositionId2 = UUID.random.toString
      val sentenceId1 = UUID.random.toString
      val sentenceId2 = UUID.random.toString
      //val knowledge1 = Knowledge(sentenceA,"ja_JP", "{}", false)
      //val knowledge2 = Knowledge(sentenceB,"ja_JP", "{}", false)
      val knowledge3 = Knowledge(sentenceC,"ja_JP", "{}", false)
      val knowledge4 = Knowledge(sentenceD,"ja_JP", "{}", false)

      val paraphrase1 = Knowledge(paraphraseA,"ja_JP", "{}", false)
      val paraphrase2 = Knowledge(paraphraseB,"ja_JP", "{}", false)
      val paraphrase3 = Knowledge(paraphraseC,"ja_JP", "{}", false)
      val paraphrase4 = Knowledge(paraphraseD,"ja_JP", "{}", false)

      registerSingleClaim(KnowledgeForParser(propositionId1, sentenceId1, knowledge3), transversalState)
      registerSingleClaim(KnowledgeForParser(propositionId2, sentenceId2, knowledge4), transversalState)

      val propositionIdForInference = UUID.random.toString
      val premiseKnowledge = List(KnowledgeForParser(propositionIdForInference, UUID.random.toString, paraphrase1), KnowledgeForParser(propositionIdForInference, UUID.random.toString, paraphrase2))
      val claimKnowledge = List(KnowledgeForParser(propositionIdForInference, UUID.random.toString, paraphrase3), KnowledgeForParser(propositionIdForInference, UUID.random.toString, paraphrase4))
      val inputSentence = Json.toJson(InputSentenceForParser(premiseKnowledge, claimKnowledge)).toString()
      val json = ToposoidUtils.callComponent(inputSentence, conf.getString("TOPOSOID_SENTENCE_PARSER_JP_WEB_HOST"), conf.getString("TOPOSOID_SENTENCE_PARSER_JP_WEB_PORT"), "analyze", transversalState)
      val fr = FakeRequest(POST, "/execute")
        .withHeaders("Content-type" -> "application/json", TRANSVERSAL_STATE.str -> transversalStateJson)
        .withJsonBody(Json.parse(json))
      val result = call(controller.execute(), fr)
      status(result) mustBe OK
      contentType(result) mustBe Some("application/json")
      val jsonResult: String = contentAsJson(result).toString()
      val analyzedSentenceObjects: AnalyzedSentenceObjects = Json.parse(jsonResult).as[AnalyzedSentenceObjects]
      assert(analyzedSentenceObjects.analyzedSentenceObjects.filter(x => x.knowledgeBaseSemiGlobalNode.sentenceType.equals(PREMISE.index) && x.deductionResult.status).size == 0)
      assert(analyzedSentenceObjects.analyzedSentenceObjects.filter(x => x.knowledgeBaseSemiGlobalNode.sentenceType.equals(CLAIM.index) && x.deductionResult.status).size == 2)
      assert(analyzedSentenceObjects.analyzedSentenceObjects.filter(x => x.knowledgeBaseSemiGlobalNode.sentenceType.equals(CLAIM.index) && x.deductionResult.havePremiseInGivenProposition).size == 0)
      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId1, featureId = sentenceId1, sentenceType = CLAIM.index, lang = "ja_JP", PROPOSITION_ID.index, UNSPECIFIED.index), transversalState)
      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId2, featureId = sentenceId2, sentenceType = CLAIM.index, lang = "ja_JP", PROPOSITION_ID.index, UNSPECIFIED.index), transversalState)

    }
  }

  "The specification33" should {
    "returns an appropriate response" in {
      val propositionId1 = UUID.random.toString
      val sentenceId1 = UUID.random.toString
      val sentenceId2 = UUID.random.toString
      val knowledge1 = Knowledge(sentenceA,"ja_JP", "{}", false)
      //val knowledge2 = Knowledge(sentenceB,"ja_JP", "{}", false)
      val knowledge3 = Knowledge(sentenceC,"ja_JP", "{}", false)
      //val knowledge4 = Knowledge(sentenceD,"ja_JP", "{}", false)

      val paraphrase1 = Knowledge(paraphraseA,"ja_JP", "{}", false)
      val paraphrase2 = Knowledge(paraphraseB,"ja_JP", "{}", false)
      val paraphrase3 = Knowledge(paraphraseC,"ja_JP", "{}", false)
      val paraphrase4 = Knowledge(paraphraseD,"ja_JP", "{}", false)

      val knowledgeSentenceSetForParser = KnowledgeSentenceSetForParser(
        List(KnowledgeForParser(propositionId1, sentenceId1, knowledge1)),
        List.empty[PropositionRelation],
        List(KnowledgeForParser(propositionId1, sentenceId2, knowledge3)),
        List.empty[PropositionRelation]
      )
      TestUtils.registerData(knowledgeSentenceSetForParser, transversalState)
      FeatureVectorizer.createVector(knowledgeSentenceSetForParser, transversalState)
      Thread.sleep(5000)

      val propositionIdForInference = UUID.random.toString
      val premiseKnowledge = List(KnowledgeForParser(propositionIdForInference, UUID.random.toString, paraphrase1), KnowledgeForParser(propositionIdForInference, UUID.random.toString, paraphrase2))
      val claimKnowledge = List(KnowledgeForParser(propositionIdForInference, UUID.random.toString, paraphrase3), KnowledgeForParser(propositionIdForInference, UUID.random.toString, paraphrase4))
      val inputSentence = Json.toJson(InputSentenceForParser(premiseKnowledge, claimKnowledge)).toString()

      val json = ToposoidUtils.callComponent(inputSentence, conf.getString("TOPOSOID_SENTENCE_PARSER_JP_WEB_HOST"), conf.getString("TOPOSOID_SENTENCE_PARSER_JP_WEB_PORT"), "analyze", transversalState)
      val fr = FakeRequest(POST, "/execute")
        .withHeaders("Content-type" -> "application/json", TRANSVERSAL_STATE.str -> transversalStateJson)
        .withJsonBody(Json.parse(json))
      val result = call(controller.execute(), fr)
      status(result) mustBe OK
      contentType(result) mustBe Some("application/json")
      val jsonResult: String = contentAsJson(result).toString()
      val analyzedSentenceObjects: AnalyzedSentenceObjects = Json.parse(jsonResult).as[AnalyzedSentenceObjects]
      assert(analyzedSentenceObjects.analyzedSentenceObjects.filter(x => x.knowledgeBaseSemiGlobalNode.sentenceType.equals(PREMISE.index) && x.deductionResult.status).size == 0)
      assert(analyzedSentenceObjects.analyzedSentenceObjects.filter(x => x.knowledgeBaseSemiGlobalNode.sentenceType.equals(CLAIM.index) && x.deductionResult.status).size == 0)
      assert(analyzedSentenceObjects.analyzedSentenceObjects.filter(x => x.knowledgeBaseSemiGlobalNode.sentenceType.equals(CLAIM.index) && x.deductionResult.havePremiseInGivenProposition).size == 0)
      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId1, featureId = sentenceId1, sentenceType = PREMISE.index, lang = "ja_JP", PROPOSITION_ID.index, UNSPECIFIED.index), transversalState)
      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId1, featureId = sentenceId2, sentenceType = CLAIM.index, lang = "ja_JP", PROPOSITION_ID.index, UNSPECIFIED.index), transversalState)

    }
  }

  "The specification34" should {
    "returns an appropriate response" in {
      val propositionId1 = UUID.random.toString
      val sentenceId1 = UUID.random.toString
      val sentenceId2 = UUID.random.toString
      val sentenceId3 = UUID.random.toString
      val knowledge1 = Knowledge(sentenceA,"ja_JP", "{}", false)
      val knowledge2 = Knowledge(sentenceB,"ja_JP", "{}", false)
      val knowledge3 = Knowledge(sentenceC,"ja_JP", "{}", false)
      //val knowledge4 = Knowledge(sentenceD,"ja_JP", "{}", false)

      val paraphrase1 = Knowledge(paraphraseA,"ja_JP", "{}", false)
      val paraphrase2 = Knowledge(paraphraseB,"ja_JP", "{}", false)
      val paraphrase3 = Knowledge(paraphraseC,"ja_JP", "{}", false)
      val paraphrase4 = Knowledge(paraphraseD,"ja_JP", "{}", false)

      val knowledgeSentenceSetForParser = KnowledgeSentenceSetForParser(
        List(KnowledgeForParser(propositionId1, sentenceId1, knowledge1), KnowledgeForParser(propositionId1, sentenceId2, knowledge2)),
        List(PropositionRelation("AND", 0,1)),
        List(KnowledgeForParser(propositionId1, sentenceId3, knowledge3)),
        List.empty[PropositionRelation])
      TestUtils.registerData(knowledgeSentenceSetForParser, transversalState)
      FeatureVectorizer.createVector(knowledgeSentenceSetForParser, transversalState)
      Thread.sleep(5000)

      val propositionIdForInference = UUID.random.toString
      val premiseKnowledge = List(KnowledgeForParser(propositionIdForInference, UUID.random.toString, paraphrase1), KnowledgeForParser(propositionIdForInference, UUID.random.toString, paraphrase2))
      val claimKnowledge = List(KnowledgeForParser(propositionIdForInference, UUID.random.toString, paraphrase3), KnowledgeForParser(propositionIdForInference, UUID.random.toString, paraphrase4))
      val inputSentence = Json.toJson(InputSentenceForParser(premiseKnowledge, claimKnowledge)).toString()

      val json = ToposoidUtils.callComponent(inputSentence, conf.getString("TOPOSOID_SENTENCE_PARSER_JP_WEB_HOST"), conf.getString("TOPOSOID_SENTENCE_PARSER_JP_WEB_PORT"), "analyze", transversalState)
      val fr = FakeRequest(POST, "/execute")
        .withHeaders("Content-type" -> "application/json", TRANSVERSAL_STATE.str -> transversalStateJson)
        .withJsonBody(Json.parse(json))
      val result = call(controller.execute(), fr)
      status(result) mustBe OK
      contentType(result) mustBe Some("application/json")
      val jsonResult: String = contentAsJson(result).toString()
      val analyzedSentenceObjects: AnalyzedSentenceObjects = Json.parse(jsonResult).as[AnalyzedSentenceObjects]
      assert(analyzedSentenceObjects.analyzedSentenceObjects.filter(x => x.knowledgeBaseSemiGlobalNode.sentenceType.equals(PREMISE.index) && x.deductionResult.status).size == 0)
      assert(analyzedSentenceObjects.analyzedSentenceObjects.filter(x => x.knowledgeBaseSemiGlobalNode.sentenceType.equals(CLAIM.index) && x.deductionResult.status).size == 0)
      assert(analyzedSentenceObjects.analyzedSentenceObjects.filter(x => x.knowledgeBaseSemiGlobalNode.sentenceType.equals(CLAIM.index) && x.deductionResult.havePremiseInGivenProposition).size == 0)
      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId1, featureId = sentenceId1, sentenceType = PREMISE.index, lang = "ja_JP", PROPOSITION_ID.index, UNSPECIFIED.index), transversalState)
      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId1, featureId = sentenceId2, sentenceType = PREMISE.index, lang = "ja_JP", PROPOSITION_ID.index, UNSPECIFIED.index), transversalState)
      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId1, featureId = sentenceId3, sentenceType = CLAIM.index, lang = "ja_JP", PROPOSITION_ID.index, UNSPECIFIED.index), transversalState)

    }
  }

  "The specification35" should {
    "returns an appropriate response" in {
      val propositionId1 = UUID.random.toString
      val sentenceId1 = UUID.random.toString
      val sentenceId2 = UUID.random.toString
      val sentenceId3 = UUID.random.toString
      val knowledge1 = Knowledge(sentenceA,"ja_JP", "{}", false)
      //val knowledge2 = Knowledge(sentenceB,"ja_JP", "{}", false)
      val knowledge3 = Knowledge(sentenceC,"ja_JP", "{}", false)
      val knowledge4 = Knowledge(sentenceD,"ja_JP", "{}", false)

      val paraphrase1 = Knowledge(paraphraseA,"ja_JP", "{}", false)
      val paraphrase2 = Knowledge(paraphraseB,"ja_JP", "{}", false)
      val paraphrase3 = Knowledge(paraphraseC,"ja_JP", "{}", false)
      val paraphrase4 = Knowledge(paraphraseD,"ja_JP", "{}", false)

      val knowledgeSentenceSetForParser = KnowledgeSentenceSetForParser(
        List(KnowledgeForParser(propositionId1, sentenceId1, knowledge1)),
        List.empty[PropositionRelation],
        List(KnowledgeForParser(propositionId1, sentenceId2, knowledge3), KnowledgeForParser(propositionId1, sentenceId3, knowledge4)),
        List(PropositionRelation("AND", 0,1)))
      TestUtils.registerData(knowledgeSentenceSetForParser, transversalState)
      FeatureVectorizer.createVector(knowledgeSentenceSetForParser, transversalState)
      Thread.sleep(5000)

      val propositionIdForInference = UUID.random.toString
      val premiseKnowledge = List(KnowledgeForParser(propositionIdForInference, UUID.random.toString, paraphrase1), KnowledgeForParser(propositionIdForInference, UUID.random.toString, paraphrase2))
      val claimKnowledge = List(KnowledgeForParser(propositionIdForInference, UUID.random.toString, paraphrase3), KnowledgeForParser(propositionIdForInference, UUID.random.toString, paraphrase4))
      val inputSentence = Json.toJson(InputSentenceForParser(premiseKnowledge, claimKnowledge)).toString()

      val json = ToposoidUtils.callComponent(inputSentence, conf.getString("TOPOSOID_SENTENCE_PARSER_JP_WEB_HOST"), conf.getString("TOPOSOID_SENTENCE_PARSER_JP_WEB_PORT"), "analyze", transversalState)
      val fr = FakeRequest(POST, "/execute")
        .withHeaders("Content-type" -> "application/json", TRANSVERSAL_STATE.str -> transversalStateJson)
        .withJsonBody(Json.parse(json))
      val result = call(controller.execute(), fr)
      status(result) mustBe OK
      contentType(result) mustBe Some("application/json")
      val jsonResult: String = contentAsJson(result).toString()
      val analyzedSentenceObjects: AnalyzedSentenceObjects = Json.parse(jsonResult).as[AnalyzedSentenceObjects]
      assert(analyzedSentenceObjects.analyzedSentenceObjects.filter(x => x.knowledgeBaseSemiGlobalNode.sentenceType.equals(PREMISE.index) && x.deductionResult.status).size == 0)
      assert(analyzedSentenceObjects.analyzedSentenceObjects.filter(x => x.knowledgeBaseSemiGlobalNode.sentenceType.equals(CLAIM.index) && x.deductionResult.status).size == 0)
      assert(analyzedSentenceObjects.analyzedSentenceObjects.filter(x => x.knowledgeBaseSemiGlobalNode.sentenceType.equals(CLAIM.index) && x.deductionResult.havePremiseInGivenProposition).size == 0)
      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId1, featureId = sentenceId1, sentenceType = PREMISE.index, lang = "ja_JP", PROPOSITION_ID.index, UNSPECIFIED.index), transversalState)
      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId1, featureId = sentenceId2, sentenceType = CLAIM.index, lang = "ja_JP", PROPOSITION_ID.index, UNSPECIFIED.index), transversalState)
      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId1, featureId = sentenceId3, sentenceType = CLAIM.index, lang = "ja_JP", PROPOSITION_ID.index, UNSPECIFIED.index), transversalState)

    }
  }

  "The specification36" should {
    "returns an appropriate response" in {
      val propositionId1 = UUID.random.toString
      val sentenceId1 = UUID.random.toString
      val sentenceId2 = UUID.random.toString
      val sentenceId3 = UUID.random.toString
      val sentenceId4 = UUID.random.toString
      val knowledge1 = Knowledge(sentenceA,"ja_JP", "{}", false)
      val knowledge2 = Knowledge(sentenceB,"ja_JP", "{}", false)
      val knowledge3 = Knowledge(sentenceC,"ja_JP", "{}", false)
      val knowledge4 = Knowledge(sentenceD,"ja_JP", "{}", false)

      val paraphrase1 = Knowledge(paraphraseA,"ja_JP", "{}", false)
      val paraphrase2 = Knowledge(paraphraseB,"ja_JP", "{}", false)
      val paraphrase3 = Knowledge(paraphraseC,"ja_JP", "{}", false)
      val paraphrase4 = Knowledge(paraphraseD,"ja_JP", "{}", false)

      val knowledgeSentenceSetForParser = KnowledgeSentenceSetForParser(
        List(KnowledgeForParser(propositionId1, sentenceId1, knowledge1), KnowledgeForParser(propositionId1, sentenceId2, knowledge2)),
        List(PropositionRelation("AND", 0,1)),
        List(KnowledgeForParser(propositionId1, sentenceId3, knowledge3), KnowledgeForParser(propositionId1, sentenceId4, knowledge4)),
        List(PropositionRelation("AND", 0,1)))
      TestUtils.registerData(knowledgeSentenceSetForParser, transversalState)
      FeatureVectorizer.createVector(knowledgeSentenceSetForParser, transversalState)
      Thread.sleep(5000)

      val propositionIdForInference = UUID.random.toString
      val premiseKnowledge = List(KnowledgeForParser(propositionIdForInference, UUID.random.toString, paraphrase1), KnowledgeForParser(propositionIdForInference, UUID.random.toString, paraphrase2))
      val claimKnowledge = List(KnowledgeForParser(propositionIdForInference, UUID.random.toString, paraphrase3), KnowledgeForParser(propositionIdForInference, UUID.random.toString, paraphrase4))
      val inputSentence = Json.toJson(InputSentenceForParser(premiseKnowledge, claimKnowledge)).toString()
      val json = ToposoidUtils.callComponent(inputSentence, conf.getString("TOPOSOID_SENTENCE_PARSER_JP_WEB_HOST"), conf.getString("TOPOSOID_SENTENCE_PARSER_JP_WEB_PORT"), "analyze", transversalState)
      val fr = FakeRequest(POST, "/execute")
        .withHeaders("Content-type" -> "application/json", TRANSVERSAL_STATE.str -> transversalStateJson)
        .withJsonBody(Json.parse(json))
      val result = call(controller.execute(), fr)
      status(result) mustBe OK
      contentType(result) mustBe Some("application/json")
      val jsonResult: String = contentAsJson(result).toString()
      val analyzedSentenceObjects: AnalyzedSentenceObjects = Json.parse(jsonResult).as[AnalyzedSentenceObjects]
      assert(analyzedSentenceObjects.analyzedSentenceObjects.filter(x => x.knowledgeBaseSemiGlobalNode.sentenceType.equals(PREMISE.index) && x.deductionResult.status).size == 0)
      assert(analyzedSentenceObjects.analyzedSentenceObjects.filter(x => x.knowledgeBaseSemiGlobalNode.sentenceType.equals(CLAIM.index) && x.deductionResult.status).size == 0)
      assert(analyzedSentenceObjects.analyzedSentenceObjects.filter(x => x.knowledgeBaseSemiGlobalNode.sentenceType.equals(CLAIM.index) && x.deductionResult.havePremiseInGivenProposition).size == 0)
      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId1, featureId = sentenceId1, sentenceType = PREMISE.index, lang = "ja_JP", PROPOSITION_ID.index, UNSPECIFIED.index), transversalState)
      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId1, featureId = sentenceId2, sentenceType = PREMISE.index, lang = "ja_JP", PROPOSITION_ID.index, UNSPECIFIED.index), transversalState)
      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId1, featureId = sentenceId3, sentenceType = CLAIM.index, lang = "ja_JP", PROPOSITION_ID.index, UNSPECIFIED.index), transversalState)
      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId1, featureId = sentenceId4, sentenceType = CLAIM.index, lang = "ja_JP", PROPOSITION_ID.index, UNSPECIFIED.index), transversalState)
    }
  }

  "The specification37" should {
    "returns an appropriate response" in {
      val propositionId1 = UUID.random.toString
      val propositionId2 = UUID.random.toString
      val propositionId3 = UUID.random.toString
      val sentenceId1 = UUID.random.toString
      val sentenceId2 = UUID.random.toString
      val sentenceId3 = UUID.random.toString
      val sentenceId4 = UUID.random.toString
      val sentenceId5 = UUID.random.toString
      val sentenceId6 = UUID.random.toString
      val knowledge1 = Knowledge(sentenceA,"ja_JP", "{}", false)
      val knowledge2 = Knowledge(sentenceB,"ja_JP", "{}", false)
      val knowledge3 = Knowledge(sentenceC,"ja_JP", "{}", false)
      val knowledge4 = Knowledge(sentenceD,"ja_JP", "{}", false)

      val paraphrase1 = Knowledge(paraphraseA,"ja_JP", "{}", false)
      val paraphrase2 = Knowledge(paraphraseB,"ja_JP", "{}", false)
      val paraphrase3 = Knowledge(paraphraseC,"ja_JP", "{}", false)
      val paraphrase4 = Knowledge(paraphraseD,"ja_JP", "{}", false)

      registerSingleClaim(KnowledgeForParser(propositionId1, sentenceId1, knowledge1), transversalState)
      registerSingleClaim(KnowledgeForParser(propositionId2, sentenceId2, knowledge2), transversalState)

      val knowledgeSentenceSetForParser = KnowledgeSentenceSetForParser(
        List(KnowledgeForParser(propositionId3, sentenceId3, knowledge1), KnowledgeForParser(propositionId3, sentenceId4, knowledge2)),
        List(PropositionRelation("AND", 0,1)),
        List(KnowledgeForParser(propositionId3, sentenceId5, knowledge3), KnowledgeForParser(propositionId3, sentenceId6, knowledge4)),
        List(PropositionRelation("AND", 0,1)))
      TestUtils.registerData(knowledgeSentenceSetForParser, transversalState)
      FeatureVectorizer.createVector(knowledgeSentenceSetForParser, transversalState)
      Thread.sleep(5000)

      val propositionIdForInference = UUID.random.toString
      val premiseKnowledge = List(KnowledgeForParser(propositionIdForInference, UUID.random.toString, paraphrase1), KnowledgeForParser(propositionIdForInference, UUID.random.toString, paraphrase2))
      val claimKnowledge = List(KnowledgeForParser(propositionIdForInference, UUID.random.toString, paraphrase3), KnowledgeForParser(propositionIdForInference, UUID.random.toString, paraphrase4))
      val inputSentence = Json.toJson(InputSentenceForParser(premiseKnowledge, claimKnowledge)).toString()

      val json = ToposoidUtils.callComponent(inputSentence, conf.getString("TOPOSOID_SENTENCE_PARSER_JP_WEB_HOST"), conf.getString("TOPOSOID_SENTENCE_PARSER_JP_WEB_PORT"), "analyze", transversalState)
      val fr = FakeRequest(POST, "/execute")
        .withHeaders("Content-type" -> "application/json", TRANSVERSAL_STATE.str -> transversalStateJson)
        .withJsonBody(Json.parse(json))
      val result = call(controller.execute(), fr)
      status(result) mustBe OK
      contentType(result) mustBe Some("application/json")
      val jsonResult: String = contentAsJson(result).toString()
      val analyzedSentenceObjects: AnalyzedSentenceObjects = Json.parse(jsonResult).as[AnalyzedSentenceObjects]
      assert(analyzedSentenceObjects.analyzedSentenceObjects.filter(x => x.knowledgeBaseSemiGlobalNode.sentenceType.equals(PREMISE.index) && x.deductionResult.status).size == 2)
      assert(analyzedSentenceObjects.analyzedSentenceObjects.filter(x => x.knowledgeBaseSemiGlobalNode.sentenceType.equals(CLAIM.index) && x.deductionResult.status).size == 2)
      assert(analyzedSentenceObjects.analyzedSentenceObjects.filter(x => x.knowledgeBaseSemiGlobalNode.sentenceType.equals(CLAIM.index) && x.deductionResult.havePremiseInGivenProposition).size == 2)

      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId1, featureId = sentenceId1, sentenceType = CLAIM.index, lang = "ja_JP", PROPOSITION_ID.index, UNSPECIFIED.index), transversalState)
      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId2, featureId = sentenceId2, sentenceType = CLAIM.index, lang = "ja_JP", PROPOSITION_ID.index, UNSPECIFIED.index), transversalState)
      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId3, featureId = sentenceId3, sentenceType = PREMISE.index, lang = "ja_JP", PROPOSITION_ID.index, UNSPECIFIED.index), transversalState)
      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId3, featureId = sentenceId4, sentenceType = PREMISE.index, lang = "ja_JP", PROPOSITION_ID.index, UNSPECIFIED.index), transversalState)
      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId3, featureId = sentenceId5, sentenceType = CLAIM.index, lang = "ja_JP", PROPOSITION_ID.index, UNSPECIFIED.index), transversalState)
      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId3, featureId = sentenceId6, sentenceType = CLAIM.index, lang = "ja_JP", PROPOSITION_ID.index, UNSPECIFIED.index), transversalState)
    }
  }

  "The specification37A" should {
    "returns an appropriate response" in {
      val propositionId1 = UUID.random.toString
      val propositionId2 = UUID.random.toString
      val propositionId3 = UUID.random.toString
      val propositionId4 = UUID.random.toString
      val sentenceId1 = UUID.random.toString
      val sentenceId2 = UUID.random.toString
      val sentenceId3 = UUID.random.toString
      val sentenceId4 = UUID.random.toString
      val sentenceId5 = UUID.random.toString
      val sentenceId6 = UUID.random.toString
      val knowledge1 = Knowledge(sentenceA, "ja_JP", "{}", false)
      val knowledge2 = Knowledge(sentenceB, "ja_JP", "{}", false)
      val knowledge3 = Knowledge(sentenceC, "ja_JP", "{}", false)
      val knowledge4 = Knowledge(sentenceD, "ja_JP", "{}", false)

      val paraphrase1 = Knowledge(paraphraseA, "ja_JP", "{}", false)
      val paraphrase2 = Knowledge(paraphraseB, "ja_JP", "{}", false)
      val paraphrase3 = Knowledge(paraphraseC, "ja_JP", "{}", false)
      val paraphrase4 = Knowledge(paraphraseD, "ja_JP", "{}", false)

      registerSingleClaim(KnowledgeForParser(propositionId1, sentenceId1, knowledge1), transversalState)
      registerSingleClaim(KnowledgeForParser(propositionId2, sentenceId2, knowledge2), transversalState)
      registerSingleClaim(KnowledgeForParser(propositionId3, sentenceId3, knowledge3), transversalState)
      registerSingleClaim(KnowledgeForParser(propositionId4, sentenceId4, knowledge4), transversalState)

      val propositionIdForInference = UUID.random.toString
      val premiseKnowledge = List(KnowledgeForParser(propositionIdForInference, UUID.random.toString, paraphrase1), KnowledgeForParser(propositionIdForInference, UUID.random.toString, paraphrase2))
      val claimKnowledge = List(KnowledgeForParser(propositionIdForInference, UUID.random.toString, paraphrase3), KnowledgeForParser(propositionIdForInference, UUID.random.toString, paraphrase4))
      val inputSentence = Json.toJson(InputSentenceForParser(premiseKnowledge, claimKnowledge)).toString()

      val json = ToposoidUtils.callComponent(inputSentence, conf.getString("TOPOSOID_SENTENCE_PARSER_JP_WEB_HOST"), conf.getString("TOPOSOID_SENTENCE_PARSER_JP_WEB_PORT"), "analyze", transversalState)
      val fr = FakeRequest(POST, "/execute")
        .withHeaders("Content-type" -> "application/json", TRANSVERSAL_STATE.str -> transversalStateJson)
        .withJsonBody(Json.parse(json))
      val result = call(controller.execute(), fr)
      status(result) mustBe OK
      contentType(result) mustBe Some("application/json")
      val jsonResult: String = contentAsJson(result).toString()
      val analyzedSentenceObjects: AnalyzedSentenceObjects = Json.parse(jsonResult).as[AnalyzedSentenceObjects]
      assert(analyzedSentenceObjects.analyzedSentenceObjects.filter(x => x.knowledgeBaseSemiGlobalNode.sentenceType.equals(PREMISE.index) && x.deductionResult.status).size == 2)
      assert(analyzedSentenceObjects.analyzedSentenceObjects.filter(x => x.knowledgeBaseSemiGlobalNode.sentenceType.equals(CLAIM.index) && x.deductionResult.status).size == 2)
      assert(analyzedSentenceObjects.analyzedSentenceObjects.filter(x => x.knowledgeBaseSemiGlobalNode.sentenceType.equals(CLAIM.index) && x.deductionResult.havePremiseInGivenProposition).size == 0)

      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId1, featureId = sentenceId1, sentenceType = CLAIM.index, lang = "ja_JP", PROPOSITION_ID.index, UNSPECIFIED.index), transversalState)
      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId2, featureId = sentenceId2, sentenceType = CLAIM.index, lang = "ja_JP", PROPOSITION_ID.index, UNSPECIFIED.index), transversalState)
      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId3, featureId = sentenceId3, sentenceType = CLAIM.index, lang = "ja_JP", PROPOSITION_ID.index, UNSPECIFIED.index), transversalState)
      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId4, featureId = sentenceId4, sentenceType = CLAIM.index, lang = "ja_JP", PROPOSITION_ID.index, UNSPECIFIED.index), transversalState)

    }
  }


  "The specification38" should {
    "returns an appropriate response" in {
      val propositionId1 = UUID.random.toString
      val propositionId2 = UUID.random.toString
      val sentenceId1 = UUID.random.toString
      val sentenceId2 = UUID.random.toString
      val sentenceId3 = UUID.random.toString
      val sentenceId4 = UUID.random.toString
      val sentenceId5 = UUID.random.toString
      val knowledge1 = Knowledge(sentenceA,"ja_JP", "{}", false)
      val knowledge2 = Knowledge(sentenceB,"ja_JP", "{}", false)
      val knowledge3 = Knowledge(sentenceC,"ja_JP", "{}", false)
      val knowledge4 = Knowledge(sentenceD,"ja_JP", "{}", false)

      val paraphrase1 = Knowledge(paraphraseA,"ja_JP", "{}", false)
      val paraphrase2 = Knowledge(paraphraseB,"ja_JP", "{}", false)
      val paraphrase3 = Knowledge(paraphraseC,"ja_JP", "{}", false)
      val paraphrase4 = Knowledge(paraphraseD,"ja_JP", "{}", false)

      registerSingleClaim(KnowledgeForParser(propositionId1, sentenceId1, knowledge1), transversalState)

      val knowledgeSentenceSetForParser = KnowledgeSentenceSetForParser(
        List(KnowledgeForParser(propositionId2, sentenceId2, knowledge1), KnowledgeForParser(propositionId2, sentenceId3, knowledge2)),
        List(PropositionRelation("AND", 0,1)),
        List(KnowledgeForParser(propositionId2, sentenceId4, knowledge3), KnowledgeForParser(propositionId2, sentenceId5, knowledge4)),
        List(PropositionRelation("AND", 0,1)))
      TestUtils.registerData(knowledgeSentenceSetForParser, transversalState)
      FeatureVectorizer.createVector(knowledgeSentenceSetForParser, transversalState)
      Thread.sleep(5000)

      val propositionIdForInference = UUID.random.toString
      val premiseKnowledge = List(KnowledgeForParser(propositionIdForInference, UUID.random.toString, paraphrase1), KnowledgeForParser(propositionIdForInference, UUID.random.toString, paraphrase2))
      val claimKnowledge = List(KnowledgeForParser(propositionIdForInference, UUID.random.toString, paraphrase3), KnowledgeForParser(propositionIdForInference, UUID.random.toString, paraphrase4))
      val inputSentence = Json.toJson(InputSentenceForParser(premiseKnowledge, claimKnowledge)).toString()

      val json = ToposoidUtils.callComponent(inputSentence, conf.getString("TOPOSOID_SENTENCE_PARSER_JP_WEB_HOST"), conf.getString("TOPOSOID_SENTENCE_PARSER_JP_WEB_PORT"), "analyze", transversalState)
      val fr = FakeRequest(POST, "/execute")
        .withHeaders("Content-type" -> "application/json", TRANSVERSAL_STATE.str -> transversalStateJson)
        .withJsonBody(Json.parse(json))
      val result = call(controller.execute(), fr)
      status(result) mustBe OK
      contentType(result) mustBe Some("application/json")
      val jsonResult: String = contentAsJson(result).toString()
      val analyzedSentenceObjects: AnalyzedSentenceObjects = Json.parse(jsonResult).as[AnalyzedSentenceObjects]
      assert(analyzedSentenceObjects.analyzedSentenceObjects.filter(x => x.knowledgeBaseSemiGlobalNode.sentenceType.equals(PREMISE.index) && x.deductionResult.status).size == 1)
      assert(analyzedSentenceObjects.analyzedSentenceObjects.filter(x => x.knowledgeBaseSemiGlobalNode.sentenceType.equals(CLAIM.index) && x.deductionResult.status).size == 0)
      assert(analyzedSentenceObjects.analyzedSentenceObjects.filter(x => x.knowledgeBaseSemiGlobalNode.sentenceType.equals(CLAIM.index) && x.deductionResult.havePremiseInGivenProposition).size == 0)


      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId1, featureId = sentenceId1, sentenceType = CLAIM.index, lang = "ja_JP", PROPOSITION_ID.index, UNSPECIFIED.index), transversalState)
      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId2, featureId = sentenceId2, sentenceType = PREMISE.index, lang = "ja_JP", PROPOSITION_ID.index, UNSPECIFIED.index), transversalState)
      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId2, featureId = sentenceId3, sentenceType = PREMISE.index, lang = "ja_JP", PROPOSITION_ID.index, UNSPECIFIED.index), transversalState)
      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId2, featureId = sentenceId4, sentenceType = CLAIM.index, lang = "ja_JP", PROPOSITION_ID.index, UNSPECIFIED.index), transversalState)
      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId2, featureId = sentenceId5, sentenceType = CLAIM.index, lang = "ja_JP", PROPOSITION_ID.index, UNSPECIFIED.index), transversalState)

    }
  }

  "The specification39" should {
    "returns an appropriate response" in {
      val propositionId1 = UUID.random.toString
      val propositionId2 = UUID.random.toString
      val sentenceId1 = UUID.random.toString
      val sentenceId2 = UUID.random.toString
      val sentenceId3 = UUID.random.toString
      val sentenceId4 = UUID.random.toString
      val sentenceId5 = UUID.random.toString
      val knowledge1 = Knowledge(sentenceA,"ja_JP", "{}", false)
      val knowledge2 = Knowledge(sentenceB,"ja_JP", "{}", false)
      val knowledge3 = Knowledge(sentenceC,"ja_JP", "{}", false)
      val knowledge4 = Knowledge(sentenceD,"ja_JP", "{}", false)

      val paraphrase1 = Knowledge(paraphraseA,"ja_JP", "{}", false)
      val paraphrase2 = Knowledge(paraphraseB,"ja_JP", "{}", false)
      val paraphrase3 = Knowledge(paraphraseC,"ja_JP", "{}", false)
      val paraphrase4 = Knowledge(paraphraseD,"ja_JP", "{}", false)

      registerSingleClaim(KnowledgeForParser(propositionId1, sentenceId1, knowledge3), transversalState)

      val knowledgeSentenceSetForParser = KnowledgeSentenceSetForParser(
        List(KnowledgeForParser(propositionId2, sentenceId2, knowledge1), KnowledgeForParser(propositionId2, sentenceId3, knowledge2)),
        List(PropositionRelation("AND", 0,1)),
        List(KnowledgeForParser(propositionId2, sentenceId4, knowledge3), KnowledgeForParser(propositionId2, sentenceId5, knowledge4)),
        List(PropositionRelation("AND", 0,1)))
      TestUtils.registerData(knowledgeSentenceSetForParser, transversalState)
      FeatureVectorizer.createVector(knowledgeSentenceSetForParser, transversalState)
      Thread.sleep(5000)

      val propositionIdForInference = UUID.random.toString
      val premiseKnowledge = List(KnowledgeForParser(propositionIdForInference, UUID.random.toString, paraphrase1), KnowledgeForParser(propositionIdForInference, UUID.random.toString, paraphrase2))
      val claimKnowledge = List(KnowledgeForParser(propositionIdForInference, UUID.random.toString, paraphrase3), KnowledgeForParser(propositionIdForInference, UUID.random.toString, paraphrase4))
      val inputSentence = Json.toJson(InputSentenceForParser(premiseKnowledge, claimKnowledge)).toString()

      val json = ToposoidUtils.callComponent(inputSentence, conf.getString("TOPOSOID_SENTENCE_PARSER_JP_WEB_HOST"), conf.getString("TOPOSOID_SENTENCE_PARSER_JP_WEB_PORT"), "analyze", transversalState)
      val fr = FakeRequest(POST, "/execute")
        .withHeaders("Content-type" -> "application/json", TRANSVERSAL_STATE.str -> transversalStateJson)
        .withJsonBody(Json.parse(json))
      val result = call(controller.execute(), fr)
      status(result) mustBe OK
      contentType(result) mustBe Some("application/json")
      val jsonResult: String = contentAsJson(result).toString()
      val analyzedSentenceObjects: AnalyzedSentenceObjects = Json.parse(jsonResult).as[AnalyzedSentenceObjects]
      assert(analyzedSentenceObjects.analyzedSentenceObjects.filter(x => x.knowledgeBaseSemiGlobalNode.sentenceType.equals(PREMISE.index) && x.deductionResult.status).size == 0)
      assert(analyzedSentenceObjects.analyzedSentenceObjects.filter(x => x.knowledgeBaseSemiGlobalNode.sentenceType.equals(CLAIM.index) && x.deductionResult.status).size == 1)
      assert(analyzedSentenceObjects.analyzedSentenceObjects.filter(x => x.knowledgeBaseSemiGlobalNode.sentenceType.equals(CLAIM.index) && x.deductionResult.havePremiseInGivenProposition).size == 0)

      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId1, featureId = sentenceId1, sentenceType = CLAIM.index, lang = "ja_JP", PROPOSITION_ID.index, UNSPECIFIED.index), transversalState)
      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId2, featureId = sentenceId2, sentenceType = PREMISE.index, lang = "ja_JP", PROPOSITION_ID.index, UNSPECIFIED.index), transversalState)
      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId2, featureId = sentenceId3, sentenceType = PREMISE.index, lang = "ja_JP", PROPOSITION_ID.index, UNSPECIFIED.index), transversalState)
      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId2, featureId = sentenceId4, sentenceType = CLAIM.index, lang = "ja_JP", PROPOSITION_ID.index, UNSPECIFIED.index), transversalState)
      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId2, featureId = sentenceId5, sentenceType = CLAIM.index, lang = "ja_JP", PROPOSITION_ID.index, UNSPECIFIED.index), transversalState)

    }
  }

  "The specification40" should {
    "returns an appropriate response" in {
      val propositionId1 = UUID.random.toString
      val propositionId2 = UUID.random.toString
      val propositionId3 = UUID.random.toString
      val sentenceId1 = UUID.random.toString
      val sentenceId2 = UUID.random.toString
      val sentenceId3 = UUID.random.toString
      val sentenceId4 = UUID.random.toString
      val sentenceId5 = UUID.random.toString
      val sentenceId6 = UUID.random.toString
      val knowledge1 = Knowledge(sentenceA,"ja_JP", "{}", false)
      val knowledge2 = Knowledge(sentenceB,"ja_JP", "{}", false)
      val knowledge3 = Knowledge(sentenceC,"ja_JP", "{}", false)
      val knowledge4 = Knowledge(sentenceD,"ja_JP", "{}", false)

      val paraphrase1 = Knowledge(paraphraseA,"ja_JP", "{}", false)
      val paraphrase2 = Knowledge(paraphraseB,"ja_JP", "{}", false)
      val paraphrase3 = Knowledge(paraphraseC,"ja_JP", "{}", false)
      val paraphrase4 = Knowledge(paraphraseD,"ja_JP", "{}", false)

      registerSingleClaim(KnowledgeForParser(propositionId1, sentenceId1, knowledge1), transversalState)
      registerSingleClaim(KnowledgeForParser(propositionId2, sentenceId2, knowledge3), transversalState)

      val knowledgeSentenceSetForParser = KnowledgeSentenceSetForParser(
        List(KnowledgeForParser(propositionId3, sentenceId3, knowledge1), KnowledgeForParser(propositionId3, sentenceId4, knowledge2)),
        List(PropositionRelation("AND", 0,1)),
        List(KnowledgeForParser(propositionId3, sentenceId5, knowledge3), KnowledgeForParser(propositionId3, sentenceId6, knowledge4)),
        List(PropositionRelation("AND", 0,1)))
      TestUtils.registerData(knowledgeSentenceSetForParser, transversalState)
      FeatureVectorizer.createVector(knowledgeSentenceSetForParser, transversalState)
      Thread.sleep(5000)

      val propositionIdForInference = UUID.random.toString
      val premiseKnowledge = List(KnowledgeForParser(propositionIdForInference, UUID.random.toString, paraphrase1), KnowledgeForParser(propositionIdForInference, UUID.random.toString, paraphrase2))
      val claimKnowledge = List(KnowledgeForParser(propositionIdForInference, UUID.random.toString, paraphrase3), KnowledgeForParser(propositionIdForInference, UUID.random.toString, paraphrase4))
      val inputSentence = Json.toJson(InputSentenceForParser(premiseKnowledge, claimKnowledge)).toString()
      val json = ToposoidUtils.callComponent(inputSentence, conf.getString("TOPOSOID_SENTENCE_PARSER_JP_WEB_HOST"), conf.getString("TOPOSOID_SENTENCE_PARSER_JP_WEB_PORT"), "analyze", transversalState)
      val fr = FakeRequest(POST, "/execute")
        .withHeaders("Content-type" -> "application/json", TRANSVERSAL_STATE.str -> transversalStateJson)
        .withJsonBody(Json.parse(json))
      val result = call(controller.execute(), fr)
      status(result) mustBe OK
      contentType(result) mustBe Some("application/json")
      val jsonResult: String = contentAsJson(result).toString()
      val analyzedSentenceObjects: AnalyzedSentenceObjects = Json.parse(jsonResult).as[AnalyzedSentenceObjects]
      assert(analyzedSentenceObjects.analyzedSentenceObjects.filter(x => x.knowledgeBaseSemiGlobalNode.sentenceType.equals(PREMISE.index) && x.deductionResult.status).size == 1)
      assert(analyzedSentenceObjects.analyzedSentenceObjects.filter(x => x.knowledgeBaseSemiGlobalNode.sentenceType.equals(CLAIM.index) && x.deductionResult.status).size == 1)
      assert(analyzedSentenceObjects.analyzedSentenceObjects.filter(x => x.knowledgeBaseSemiGlobalNode.sentenceType.equals(CLAIM.index) && x.deductionResult.havePremiseInGivenProposition).size == 0)

      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId1, featureId = sentenceId1, sentenceType = CLAIM.index, lang = "ja_JP", PROPOSITION_ID.index, UNSPECIFIED.index), transversalState)
      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId2, featureId = sentenceId2, sentenceType = CLAIM.index, lang = "ja_JP", PROPOSITION_ID.index, UNSPECIFIED.index), transversalState)
      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId3, featureId = sentenceId3, sentenceType = PREMISE.index, lang = "ja_JP", PROPOSITION_ID.index, UNSPECIFIED.index), transversalState)
      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId3, featureId = sentenceId4, sentenceType = PREMISE.index, lang = "ja_JP", PROPOSITION_ID.index, UNSPECIFIED.index), transversalState)
      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId3, featureId = sentenceId5, sentenceType = CLAIM.index, lang = "ja_JP", PROPOSITION_ID.index, UNSPECIFIED.index), transversalState)
      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId3, featureId = sentenceId6, sentenceType = CLAIM.index, lang = "ja_JP", PROPOSITION_ID.index, UNSPECIFIED.index), transversalState)

    }
  }
}
