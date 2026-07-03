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

import org.apache.pekko.util.Timeout
import com.ideal.linked.common.DeploymentConverter.conf
import com.ideal.linked.toposoid.common.{SentenceType, SuperiorType, NonSentenceType, CaseGroupType, TRANSVERSAL_STATE, ToposoidUtils, TransversalState}
import com.ideal.linked.toposoid.knowledgebase.featurevector.model.FeatureVectorIdentifier
import com.ideal.linked.toposoid.knowledgebase.regist.model.{Knowledge, PropositionRelation}
import com.ideal.linked.toposoid.protocol.model.base.AnalyzedSentenceObjects
import com.ideal.linked.toposoid.protocol.model.parser.{InputSentenceForParser, KnowledgeForParser, KnowledgeSentenceSetForParser}
import com.ideal.linked.toposoid.test.utils.TestUtils
import com.ideal.linked.toposoid.vectorizer.FeatureVectorizer
import controllers.TestUtilsEx.{deleteFeatureVector, deleteNeo4JAllData, registerSingleClaim}
//import io.jvm.uuid.UUID
import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll}
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Play.materializer
import play.api.http.Status.OK
import play.api.libs.json.Json
import play.api.test.Helpers.{POST, contentType, status, _}
import play.api.test.{FakeRequest, _}

import scala.concurrent.duration.DurationInt
import com.ideal.linked.toposoid.common.ActionModeType
import com.ideal.linked.toposoid.protocol.model.base.VerifyingEdges

class HomeControllerSpecJapanese extends PlaySpec with BeforeAndAfter with BeforeAndAfterAll with GuiceOneAppPerSuite  with DefaultAwaitTimeout with Injecting {

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

  //複数の主張(完全一致)
  "The specification1" should {
    "returns an appropriate response" in {
      val sentence1 = "自然界の法則がすべての慣性系で同じように成り立っている。"
      val sentence2 = "どの慣性系から見ても光の速さは一定である。"
      val paraphrase1 = "自然界の物理法則は例外なくどの慣性系でも成立する。"
      val paraphrase2 = "見ている慣性系によらず光速は不変である。"

      val propositionId1 = java.util.UUID.randomUUID().toString
      val sentenceId1 = java.util.UUID.randomUUID().toString
      val knowledge1 = Knowledge(sentence1,"ja_JP", "{}", false)

      val propositionId2 = java.util.UUID.randomUUID().toString
      val sentenceId2 = java.util.UUID.randomUUID().toString
      val knowledge2 = Knowledge(sentence2,"ja_JP", "{}", false)

          
      val paraphraseKnowledge1 = Knowledge(paraphrase1,"ja_JP", "{}", false)
      val paraphraseKnowledge2 = Knowledge(paraphrase2,"ja_JP", "{}", false)

      TestUtilsEx.registerSingleClaim(KnowledgeForParser(propositionId1, sentenceId1, knowledge1), transversalState)
      TestUtilsEx.registerSingleClaim(KnowledgeForParser(propositionId2, sentenceId2, knowledge2), transversalState)

      val propositionIdForInference = java.util.UUID.randomUUID().toString
      val sentenceIdForInference1 = java.util.UUID.randomUUID().toString      
      val sentenceIdForInference2 = java.util.UUID.randomUUID().toString

      val premiseKnowledge = List.empty[KnowledgeForParser]
      val claimKnowledge = List(
        KnowledgeForParser(propositionIdForInference, sentenceIdForInference1, paraphraseKnowledge1),
        KnowledgeForParser(propositionIdForInference, sentenceIdForInference2, paraphraseKnowledge2)        
      )
      val inputSentence = Json.toJson(InputSentenceForParser(premiseKnowledge, claimKnowledge, ActionModeType.DEDUCTION_MODE.index)).toString()
      val json = ToposoidUtils.callComponent(inputSentence, conf.getString("TOPOSOID_SENTENCE_PARSER_JP_WEB_HOST"), conf.getString("TOPOSOID_SENTENCE_PARSER_JP_WEB_PORT"), "analyze", transversalState)      
      val fr = FakeRequest(POST, "/execute")
        .withHeaders("Content-type" -> "application/json", TRANSVERSAL_STATE.str -> transversalStateJson)
        .withJsonBody(Json.parse(json))
      val result = call(controller.execute(), fr)
      status(result) mustBe OK
      contentType(result) mustBe Some("application/json")
      val jsonResult: String = contentAsJson(result).toString()

      val aso:AnalyzedSentenceObjects = Json.parse(json).as[AnalyzedSentenceObjects]

      val verifyingEdgesList: List[VerifyingEdges] = Json.parse(jsonResult).as[List[VerifyingEdges]]

      val correctSizes = aso.analyzedSentenceObjects.map(_.edgeList.size)
      assert(verifyingEdgesList.map(x => x.coveredPropositionEdges.size).sum == correctSizes.sum)

      TestUtils.checkMatchedBothSide(json=json, sentenceId = sentenceIdForInference1, verifyingEdgesList=verifyingEdgesList, correctSize=correctSizes(0))
      TestUtils.checkMatchedOneSide(json=json, sentenceId = sentenceIdForInference1, verifyingEdgesList=verifyingEdgesList, correctSize=0)     
      TestUtils.checkNoMatch(json=json, sentenceId = sentenceIdForInference1, verifyingEdgesList=verifyingEdgesList, correctSize=0)

      TestUtils.checkMatchedBothSide(json=json, sentenceId = sentenceIdForInference2, verifyingEdgesList=verifyingEdgesList, correctSize=correctSizes(1))
      TestUtils.checkMatchedOneSide(json=json, sentenceId = sentenceIdForInference2, verifyingEdgesList=verifyingEdgesList, correctSize=0)     
      TestUtils.checkNoMatch(json=json, sentenceId = sentenceIdForInference2, verifyingEdgesList=verifyingEdgesList, correctSize=0)

      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId1, featureId = sentenceId1, sentenceType = SentenceType.CLAIM.index, lang = "ja_JP", SuperiorType.PROPOSITION_ID.index, NonSentenceType.UNSPECIFIED.index, CaseGroupType.UNSPECIFIED.index), transversalState)
      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId2, featureId = sentenceId2, sentenceType = SentenceType.CLAIM.index, lang = "ja_JP", SuperiorType.PROPOSITION_ID.index, NonSentenceType.UNSPECIFIED.index, CaseGroupType.UNSPECIFIED.index), transversalState)
    }
  }

  //複数の主張(部分一致)
  "The specification2" should {
    "returns an appropriate response" in {

      val sentence1 = "自然界の法則がすべての慣性系で同じように成り立っている。"
      val sentence2 = "どの慣性系から見ても光の速さは一定である。"
      val paraphrase1 = "自然界の物理法則は例外なくどの慣性系でも成立する。"
      val paraphrase2 = "見ている慣性系によらず光速は一定ではない。"

      val propositionId1 = java.util.UUID.randomUUID().toString
      val sentenceId1 = java.util.UUID.randomUUID().toString
      val knowledge1 = Knowledge(sentence1,"ja_JP", "{}", false)

      val propositionId2 = java.util.UUID.randomUUID().toString
      val sentenceId2 = java.util.UUID.randomUUID().toString
      val knowledge2 = Knowledge(sentence2,"ja_JP", "{}", false)

          
      val paraphraseKnowledge1 = Knowledge(paraphrase1,"ja_JP", "{}", false)
      val paraphraseKnowledge2 = Knowledge(paraphrase2,"ja_JP", "{}", false)

      TestUtilsEx.registerSingleClaim(KnowledgeForParser(propositionId1, sentenceId1, knowledge1), transversalState)
      TestUtilsEx.registerSingleClaim(KnowledgeForParser(propositionId2, sentenceId2, knowledge2), transversalState)

      val propositionIdForInference = java.util.UUID.randomUUID().toString
      val sentenceIdForInference1 = java.util.UUID.randomUUID().toString
      val sentenceIdForInference2 = java.util.UUID.randomUUID().toString

      val premiseKnowledge = List.empty[KnowledgeForParser]
      val claimKnowledge = List(
        KnowledgeForParser(propositionIdForInference, sentenceIdForInference1, paraphraseKnowledge1),
        KnowledgeForParser(propositionIdForInference, sentenceIdForInference2, paraphraseKnowledge2)        
      )
      val inputSentence = Json.toJson(InputSentenceForParser(premiseKnowledge, claimKnowledge, ActionModeType.DEDUCTION_MODE.index)).toString()
      val json = ToposoidUtils.callComponent(inputSentence, conf.getString("TOPOSOID_SENTENCE_PARSER_JP_WEB_HOST"), conf.getString("TOPOSOID_SENTENCE_PARSER_JP_WEB_PORT"), "analyze", transversalState)      
      val fr = FakeRequest(POST, "/execute")
        .withHeaders("Content-type" -> "application/json", TRANSVERSAL_STATE.str -> transversalStateJson)
        .withJsonBody(Json.parse(json))
      val result = call(controller.execute(), fr)
      status(result) mustBe OK
      contentType(result) mustBe Some("application/json")
      val jsonResult: String = contentAsJson(result).toString()

      val aso:AnalyzedSentenceObjects = Json.parse(json).as[AnalyzedSentenceObjects]

      val verifyingEdgesList: List[VerifyingEdges] = Json.parse(jsonResult).as[List[VerifyingEdges]]

      val correctSizes = aso.analyzedSentenceObjects.map(_.edgeList.size)
      assert(verifyingEdgesList.map(x => x.coveredPropositionEdges.size).sum == correctSizes(0))

      TestUtils.checkMatchedBothSide(json=json, sentenceId = sentenceIdForInference1, verifyingEdgesList=verifyingEdgesList, correctSize=correctSizes(0))
      TestUtils.checkMatchedOneSide(json=json, sentenceId = sentenceIdForInference1, verifyingEdgesList=verifyingEdgesList, correctSize=0)     
      TestUtils.checkNoMatch(json=json, sentenceId = sentenceIdForInference1, verifyingEdgesList=verifyingEdgesList, correctSize=0)

      TestUtils.checkMatchedBothSide(json=json, sentenceId = sentenceIdForInference2, verifyingEdgesList=verifyingEdgesList, correctSize=0)
      TestUtils.checkMatchedOneSide(json=json, sentenceId = sentenceIdForInference2, verifyingEdgesList=verifyingEdgesList, correctSize=0)     
      TestUtils.checkNoMatch(json=json, sentenceId = sentenceIdForInference2, verifyingEdgesList=verifyingEdgesList, correctSize=0)

      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId1, featureId = sentenceId1, sentenceType = SentenceType.CLAIM.index, lang = "ja_JP", SuperiorType.PROPOSITION_ID.index, NonSentenceType.UNSPECIFIED.index, CaseGroupType.UNSPECIFIED.index), transversalState)
      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId2, featureId = sentenceId2, sentenceType = SentenceType.CLAIM.index, lang = "ja_JP", SuperiorType.PROPOSITION_ID.index, NonSentenceType.UNSPECIFIED.index, CaseGroupType.UNSPECIFIED.index), transversalState)
    }
  }  
  //一対の前提と主張(完全一致)
  "The specification3" should {
    "returns an appropriate response" in {
      val sentence1 = "自然界の法則がすべての慣性系で同じように成り立っている。"
      val sentence2 = "どの慣性系から見ても光の速さは一定である。"
      val paraphrase1 = "自然界の物理法則は例外なくどの慣性系でも成立する。"
      val paraphrase2 = "見ている慣性系によらず光速は不変である。"

      val propositionId1 = java.util.UUID.randomUUID().toString
      val sentenceId1 = java.util.UUID.randomUUID().toString
      val knowledge1 = Knowledge(sentence1,"ja_JP", "{}", false)

      val propositionId2 = java.util.UUID.randomUUID().toString
      val sentenceId2 = java.util.UUID.randomUUID().toString
      val knowledge2 = Knowledge(sentence2,"ja_JP", "{}", false)

          
      val paraphraseKnowledge1 = Knowledge(paraphrase1,"ja_JP", "{}", false)
      val paraphraseKnowledge2 = Knowledge(paraphrase2,"ja_JP", "{}", false)

      TestUtilsEx.registerSingleClaim(KnowledgeForParser(propositionId1, sentenceId1, knowledge1), transversalState)
      TestUtilsEx.registerSingleClaim(KnowledgeForParser(propositionId2, sentenceId2, knowledge2), transversalState)

      val propositionIdForInference = java.util.UUID.randomUUID().toString
      val sentenceIdForInference1 = java.util.UUID.randomUUID().toString      
      val sentenceIdForInference2 = java.util.UUID.randomUUID().toString

      val premiseKnowledge = List(KnowledgeForParser(propositionIdForInference, sentenceIdForInference1, paraphraseKnowledge1))
      val claimKnowledge = List(KnowledgeForParser(propositionIdForInference, sentenceIdForInference2, paraphraseKnowledge2))

      val inputSentence = Json.toJson(InputSentenceForParser(premiseKnowledge, claimKnowledge, ActionModeType.DEDUCTION_MODE.index)).toString()
      val json = ToposoidUtils.callComponent(inputSentence, conf.getString("TOPOSOID_SENTENCE_PARSER_JP_WEB_HOST"), conf.getString("TOPOSOID_SENTENCE_PARSER_JP_WEB_PORT"), "analyze", transversalState)      
      val fr = FakeRequest(POST, "/execute")
        .withHeaders("Content-type" -> "application/json", TRANSVERSAL_STATE.str -> transversalStateJson)
        .withJsonBody(Json.parse(json))
      val result = call(controller.execute(), fr)
      status(result) mustBe OK
      contentType(result) mustBe Some("application/json")
      val jsonResult: String = contentAsJson(result).toString()

      val aso:AnalyzedSentenceObjects = Json.parse(json).as[AnalyzedSentenceObjects]

      val verifyingEdgesList: List[VerifyingEdges] = Json.parse(jsonResult).as[List[VerifyingEdges]]

      val correctSizes = aso.analyzedSentenceObjects.map(_.edgeList.size)
      assert(verifyingEdgesList.map(x => x.coveredPropositionEdges.size).sum == correctSizes.sum)

      TestUtils.checkMatchedBothSide(json=json, sentenceId = sentenceIdForInference1, verifyingEdgesList=verifyingEdgesList, correctSize=correctSizes(0))
      TestUtils.checkMatchedOneSide(json=json, sentenceId = sentenceIdForInference1, verifyingEdgesList=verifyingEdgesList, correctSize=0)     
      TestUtils.checkNoMatch(json=json, sentenceId = sentenceIdForInference1, verifyingEdgesList=verifyingEdgesList, correctSize=0)

      TestUtils.checkMatchedBothSide(json=json, sentenceId = sentenceIdForInference2, verifyingEdgesList=verifyingEdgesList, correctSize=correctSizes(1))
      TestUtils.checkMatchedOneSide(json=json, sentenceId = sentenceIdForInference2, verifyingEdgesList=verifyingEdgesList, correctSize=0)     
      TestUtils.checkNoMatch(json=json, sentenceId = sentenceIdForInference2, verifyingEdgesList=verifyingEdgesList, correctSize=0)

      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId1, featureId = sentenceId1, sentenceType = SentenceType.CLAIM.index, lang = "ja_JP", SuperiorType.PROPOSITION_ID.index, NonSentenceType.UNSPECIFIED.index, CaseGroupType.UNSPECIFIED.index), transversalState)
      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId2, featureId = sentenceId2, sentenceType = SentenceType.CLAIM.index, lang = "ja_JP", SuperiorType.PROPOSITION_ID.index, NonSentenceType.UNSPECIFIED.index, CaseGroupType.UNSPECIFIED.index), transversalState)
    }
  }  
  //一対の前提と主張(部分一致)
  "The specification4" should {
    "returns an appropriate response" in {

      val sentence1 = "自然界の法則がすべての慣性系で同じように成り立っている。"
      val sentence2 = "どの慣性系から見ても光の速さは一定である。"
      val paraphrase1 = "自然界の物理法則は例外なくどの慣性系でも成立する。"
      val paraphrase2 = "見ている慣性系によらず光速は一定ではない。"

      val propositionId1 = java.util.UUID.randomUUID().toString
      val sentenceId1 = java.util.UUID.randomUUID().toString
      val knowledge1 = Knowledge(sentence1,"ja_JP", "{}", false)

      val propositionId2 = java.util.UUID.randomUUID().toString
      val sentenceId2 = java.util.UUID.randomUUID().toString
      val knowledge2 = Knowledge(sentence2,"ja_JP", "{}", false)

          
      val paraphraseKnowledge1 = Knowledge(paraphrase1,"ja_JP", "{}", false)
      val paraphraseKnowledge2 = Knowledge(paraphrase2,"ja_JP", "{}", false)

      TestUtilsEx.registerSingleClaim(KnowledgeForParser(propositionId1, sentenceId1, knowledge1), transversalState)
      TestUtilsEx.registerSingleClaim(KnowledgeForParser(propositionId2, sentenceId2, knowledge2), transversalState)

      val propositionIdForInference = java.util.UUID.randomUUID().toString
      val sentenceIdForInference1 = java.util.UUID.randomUUID().toString
      val sentenceIdForInference2 = java.util.UUID.randomUUID().toString

      val premiseKnowledge = List(KnowledgeForParser(propositionIdForInference, sentenceIdForInference1, paraphraseKnowledge1))
      val claimKnowledge = List(KnowledgeForParser(propositionIdForInference, sentenceIdForInference2, paraphraseKnowledge2))
      val inputSentence = Json.toJson(InputSentenceForParser(premiseKnowledge, claimKnowledge, ActionModeType.DEDUCTION_MODE.index)).toString()
      val json = ToposoidUtils.callComponent(inputSentence, conf.getString("TOPOSOID_SENTENCE_PARSER_JP_WEB_HOST"), conf.getString("TOPOSOID_SENTENCE_PARSER_JP_WEB_PORT"), "analyze", transversalState)      
      val fr = FakeRequest(POST, "/execute")
        .withHeaders("Content-type" -> "application/json", TRANSVERSAL_STATE.str -> transversalStateJson)
        .withJsonBody(Json.parse(json))
      val result = call(controller.execute(), fr)
      status(result) mustBe OK
      contentType(result) mustBe Some("application/json")
      val jsonResult: String = contentAsJson(result).toString()

      val aso:AnalyzedSentenceObjects = Json.parse(json).as[AnalyzedSentenceObjects]

      val verifyingEdgesList: List[VerifyingEdges] = Json.parse(jsonResult).as[List[VerifyingEdges]]

      val correctSizes = aso.analyzedSentenceObjects.map(_.edgeList.size)
      assert(verifyingEdgesList.map(x => x.coveredPropositionEdges.size).sum == correctSizes(0))

      TestUtils.checkMatchedBothSide(json=json, sentenceId = sentenceIdForInference1, verifyingEdgesList=verifyingEdgesList, correctSize=correctSizes(0))
      TestUtils.checkMatchedOneSide(json=json, sentenceId = sentenceIdForInference1, verifyingEdgesList=verifyingEdgesList, correctSize=0)     
      TestUtils.checkNoMatch(json=json, sentenceId = sentenceIdForInference1, verifyingEdgesList=verifyingEdgesList, correctSize=0)

      TestUtils.checkMatchedBothSide(json=json, sentenceId = sentenceIdForInference2, verifyingEdgesList=verifyingEdgesList, correctSize=0)
      TestUtils.checkMatchedOneSide(json=json, sentenceId = sentenceIdForInference2, verifyingEdgesList=verifyingEdgesList, correctSize=0)     
      TestUtils.checkNoMatch(json=json, sentenceId = sentenceIdForInference2, verifyingEdgesList=verifyingEdgesList, correctSize=0)

      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId1, featureId = sentenceId1, sentenceType = SentenceType.CLAIM.index, lang = "ja_JP", SuperiorType.PROPOSITION_ID.index, NonSentenceType.UNSPECIFIED.index, CaseGroupType.UNSPECIFIED.index), transversalState)
      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId2, featureId = sentenceId2, sentenceType = SentenceType.CLAIM.index, lang = "ja_JP", SuperiorType.PROPOSITION_ID.index, NonSentenceType.UNSPECIFIED.index, CaseGroupType.UNSPECIFIED.index), transversalState)
    }
  }    
  //２対の前提と主張(完全一致)
  "The specification5" should {
    "returns an appropriate response" in {
      val sentence1 = "自然界の法則がすべての慣性系で同じように成り立っている。"
      val sentence2 = "どの慣性系から見ても光の速さは一定である。"
      val sentence3 = "運動する物体の速さの上限は光の速さである。"
      val sentence4 = "特殊相対性理論では運動する物体の時間は進みかたが遅くなる。"

      val paraphrase1 = "自然界の物理法則は例外なくどの慣性系でも成立する。"
      val paraphrase2 = "見ている慣性系によらず光速は不変である。"
      val paraphrase3 = "物体の運動する速さは光の速さを超えない。"
      val paraphrase4 = "特殊相対性理論において物体は運動することにより時間がゆっくり進む。"

      val propositionId1 = java.util.UUID.randomUUID().toString
      val sentenceId1 = java.util.UUID.randomUUID().toString
      val knowledge1 = Knowledge(sentence1,"ja_JP", "{}", false)

      val propositionId2 = java.util.UUID.randomUUID().toString
      val sentenceId2 = java.util.UUID.randomUUID().toString
      val knowledge2 = Knowledge(sentence2,"ja_JP", "{}", false)

      val propositionId3 = java.util.UUID.randomUUID().toString
      val sentenceId3 = java.util.UUID.randomUUID().toString
      val knowledge3 = Knowledge(sentence3,"ja_JP", "{}", false)

      val propositionId4 = java.util.UUID.randomUUID().toString
      val sentenceId4 = java.util.UUID.randomUUID().toString
      val knowledge4 = Knowledge(sentence4,"ja_JP", "{}", false)
          
      val paraphraseKnowledge1 = Knowledge(paraphrase1,"ja_JP", "{}", false)
      val paraphraseKnowledge2 = Knowledge(paraphrase2,"ja_JP", "{}", false)
      val paraphraseKnowledge3 = Knowledge(paraphrase3,"ja_JP", "{}", false)
      val paraphraseKnowledge4 = Knowledge(paraphrase4,"ja_JP", "{}", false)

      TestUtilsEx.registerSingleClaim(KnowledgeForParser(propositionId1, sentenceId1, knowledge1), transversalState)
      TestUtilsEx.registerSingleClaim(KnowledgeForParser(propositionId2, sentenceId2, knowledge2), transversalState)
      TestUtilsEx.registerSingleClaim(KnowledgeForParser(propositionId3, sentenceId3, knowledge3), transversalState)
      TestUtilsEx.registerSingleClaim(KnowledgeForParser(propositionId4, sentenceId4, knowledge4), transversalState)

      val propositionIdForInference = java.util.UUID.randomUUID().toString
      val sentenceIdForInference1 = java.util.UUID.randomUUID().toString      
      val sentenceIdForInference2 = java.util.UUID.randomUUID().toString
      val sentenceIdForInference3 = java.util.UUID.randomUUID().toString      
      val sentenceIdForInference4 = java.util.UUID.randomUUID().toString

      val premiseKnowledge = List(KnowledgeForParser(propositionIdForInference, sentenceIdForInference1, paraphraseKnowledge1), KnowledgeForParser(propositionIdForInference, sentenceIdForInference2, paraphraseKnowledge2))
      val claimKnowledge = List(KnowledgeForParser(propositionIdForInference, sentenceIdForInference3, paraphraseKnowledge3), KnowledgeForParser(propositionIdForInference, sentenceIdForInference4, paraphraseKnowledge4))

      val inputSentence = Json.toJson(InputSentenceForParser(premiseKnowledge, claimKnowledge, ActionModeType.DEDUCTION_MODE.index)).toString()
      val json = ToposoidUtils.callComponent(inputSentence, conf.getString("TOPOSOID_SENTENCE_PARSER_JP_WEB_HOST"), conf.getString("TOPOSOID_SENTENCE_PARSER_JP_WEB_PORT"), "analyze", transversalState)      
      val fr = FakeRequest(POST, "/execute")
        .withHeaders("Content-type" -> "application/json", TRANSVERSAL_STATE.str -> transversalStateJson)
        .withJsonBody(Json.parse(json))
      val result = call(controller.execute(), fr)
      status(result) mustBe OK
      contentType(result) mustBe Some("application/json")
      val jsonResult: String = contentAsJson(result).toString()

      val aso:AnalyzedSentenceObjects = Json.parse(json).as[AnalyzedSentenceObjects]
      val verifyingEdgesList: List[VerifyingEdges] = Json.parse(jsonResult).as[List[VerifyingEdges]]

      val correctSizes = aso.analyzedSentenceObjects.map(_.edgeList.size)
      assert(verifyingEdgesList.map(x => x.coveredPropositionEdges.size).sum == correctSizes.sum)

      TestUtils.checkMatchedBothSide(json=json, sentenceId = sentenceIdForInference1, verifyingEdgesList=verifyingEdgesList, correctSize=correctSizes(0))
      TestUtils.checkMatchedOneSide(json=json, sentenceId = sentenceIdForInference1, verifyingEdgesList=verifyingEdgesList, correctSize=0)     
      TestUtils.checkNoMatch(json=json, sentenceId = sentenceIdForInference1, verifyingEdgesList=verifyingEdgesList, correctSize=0)

      TestUtils.checkMatchedBothSide(json=json, sentenceId = sentenceIdForInference2, verifyingEdgesList=verifyingEdgesList, correctSize=correctSizes(1))
      TestUtils.checkMatchedOneSide(json=json, sentenceId = sentenceIdForInference2, verifyingEdgesList=verifyingEdgesList, correctSize=0)     
      TestUtils.checkNoMatch(json=json, sentenceId = sentenceIdForInference2, verifyingEdgesList=verifyingEdgesList, correctSize=0)

      TestUtils.checkMatchedBothSide(json=json, sentenceId = sentenceIdForInference3, verifyingEdgesList=verifyingEdgesList, correctSize=correctSizes(2))
      TestUtils.checkMatchedOneSide(json=json, sentenceId = sentenceIdForInference3, verifyingEdgesList=verifyingEdgesList, correctSize=0)     
      TestUtils.checkNoMatch(json=json, sentenceId = sentenceIdForInference3, verifyingEdgesList=verifyingEdgesList, correctSize=0)

      TestUtils.checkMatchedBothSide(json=json, sentenceId = sentenceIdForInference4, verifyingEdgesList=verifyingEdgesList, correctSize=correctSizes(3))
      TestUtils.checkMatchedOneSide(json=json, sentenceId = sentenceIdForInference4, verifyingEdgesList=verifyingEdgesList, correctSize=0)     
      TestUtils.checkNoMatch(json=json, sentenceId = sentenceIdForInference4, verifyingEdgesList=verifyingEdgesList, correctSize=0)

      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId1, featureId = sentenceId1, sentenceType = SentenceType.CLAIM.index, lang = "ja_JP", SuperiorType.PROPOSITION_ID.index, NonSentenceType.UNSPECIFIED.index, CaseGroupType.UNSPECIFIED.index), transversalState)
      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId2, featureId = sentenceId2, sentenceType = SentenceType.CLAIM.index, lang = "ja_JP", SuperiorType.PROPOSITION_ID.index, NonSentenceType.UNSPECIFIED.index, CaseGroupType.UNSPECIFIED.index), transversalState)
      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId2, featureId = sentenceId3, sentenceType = SentenceType.CLAIM.index, lang = "ja_JP", SuperiorType.PROPOSITION_ID.index, NonSentenceType.UNSPECIFIED.index, CaseGroupType.UNSPECIFIED.index), transversalState)
      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId2, featureId = sentenceId4, sentenceType = SentenceType.CLAIM.index, lang = "ja_JP", SuperiorType.PROPOSITION_ID.index, NonSentenceType.UNSPECIFIED.index, CaseGroupType.UNSPECIFIED.index), transversalState)
    }
  }   
  //２対の前提と主張(部分一致)
  "The specification6" should {
    "returns an appropriate response" in {
      val sentence1 = "自然界の法則がすべての慣性系で同じように成り立っている。"
      val sentence2 = "どの慣性系から見ても光の速さは一定である。"
      val sentence3 = "運動する物体の速さの上限は光の速さである。"
      val sentence4 = "特殊相対性理論では運動する物体の時間は進みかたが遅くなる。"

      val paraphrase1 = "自然界の物理法則は例外なくどの慣性系でも成立する。"
      val paraphrase2 = "見ている慣性系によらず光速は一定ではない。"
      val paraphrase3 = "物体の運動する速さは光の速さを超えない。"
      val paraphrase4 = "一般相対性理論において等価原理は大事だ。"

      val propositionId1 = java.util.UUID.randomUUID().toString
      val sentenceId1 = java.util.UUID.randomUUID().toString
      val knowledge1 = Knowledge(sentence1,"ja_JP", "{}", false)

      val propositionId2 = java.util.UUID.randomUUID().toString
      val sentenceId2 = java.util.UUID.randomUUID().toString
      val knowledge2 = Knowledge(sentence2,"ja_JP", "{}", false)

      val propositionId3 = java.util.UUID.randomUUID().toString
      val sentenceId3 = java.util.UUID.randomUUID().toString
      val knowledge3 = Knowledge(sentence3,"ja_JP", "{}", false)

      val propositionId4 = java.util.UUID.randomUUID().toString
      val sentenceId4 = java.util.UUID.randomUUID().toString
      val knowledge4 = Knowledge(sentence4,"ja_JP", "{}", false)
          
      val paraphraseKnowledge1 = Knowledge(paraphrase1,"ja_JP", "{}", false)
      val paraphraseKnowledge2 = Knowledge(paraphrase2,"ja_JP", "{}", false)
      val paraphraseKnowledge3 = Knowledge(paraphrase3,"ja_JP", "{}", false)
      val paraphraseKnowledge4 = Knowledge(paraphrase4,"ja_JP", "{}", false)

      TestUtilsEx.registerSingleClaim(KnowledgeForParser(propositionId1, sentenceId1, knowledge1), transversalState)
      TestUtilsEx.registerSingleClaim(KnowledgeForParser(propositionId2, sentenceId2, knowledge2), transversalState)
      TestUtilsEx.registerSingleClaim(KnowledgeForParser(propositionId3, sentenceId3, knowledge3), transversalState)
      TestUtilsEx.registerSingleClaim(KnowledgeForParser(propositionId4, sentenceId4, knowledge4), transversalState)

      val propositionIdForInference = java.util.UUID.randomUUID().toString
      val sentenceIdForInference1 = java.util.UUID.randomUUID().toString      
      val sentenceIdForInference2 = java.util.UUID.randomUUID().toString
      val sentenceIdForInference3 = java.util.UUID.randomUUID().toString      
      val sentenceIdForInference4 = java.util.UUID.randomUUID().toString

      val premiseKnowledge = List(KnowledgeForParser(propositionIdForInference, sentenceIdForInference1, paraphraseKnowledge1), KnowledgeForParser(propositionIdForInference, sentenceIdForInference2, paraphraseKnowledge2))
      val claimKnowledge = List(KnowledgeForParser(propositionIdForInference, sentenceIdForInference3, paraphraseKnowledge3), KnowledgeForParser(propositionIdForInference, sentenceIdForInference4, paraphraseKnowledge4))

      val inputSentence = Json.toJson(InputSentenceForParser(premiseKnowledge, claimKnowledge, ActionModeType.DEDUCTION_MODE.index)).toString()
      val json = ToposoidUtils.callComponent(inputSentence, conf.getString("TOPOSOID_SENTENCE_PARSER_JP_WEB_HOST"), conf.getString("TOPOSOID_SENTENCE_PARSER_JP_WEB_PORT"), "analyze", transversalState)      
      val fr = FakeRequest(POST, "/execute")
        .withHeaders("Content-type" -> "application/json", TRANSVERSAL_STATE.str -> transversalStateJson)
        .withJsonBody(Json.parse(json))
      val result = call(controller.execute(), fr)
      status(result) mustBe OK
      contentType(result) mustBe Some("application/json")
      val jsonResult: String = contentAsJson(result).toString()

      val aso:AnalyzedSentenceObjects = Json.parse(json).as[AnalyzedSentenceObjects]
      val verifyingEdgesList: List[VerifyingEdges] = Json.parse(jsonResult).as[List[VerifyingEdges]]

      val correctSizes = aso.analyzedSentenceObjects.map(_.edgeList.size)
      assert(verifyingEdgesList.map(x => x.coveredPropositionEdges.size).sum == correctSizes(0) + correctSizes(2))

      TestUtils.checkMatchedBothSide(json=json, sentenceId = sentenceIdForInference1, verifyingEdgesList=verifyingEdgesList, correctSize=correctSizes(0))
      TestUtils.checkMatchedOneSide(json=json, sentenceId = sentenceIdForInference1, verifyingEdgesList=verifyingEdgesList, correctSize=0)     
      TestUtils.checkNoMatch(json=json, sentenceId = sentenceIdForInference1, verifyingEdgesList=verifyingEdgesList, correctSize=0)

      TestUtils.checkMatchedBothSide(json=json, sentenceId = sentenceIdForInference2, verifyingEdgesList=verifyingEdgesList, correctSize=0)
      TestUtils.checkMatchedOneSide(json=json, sentenceId = sentenceIdForInference2, verifyingEdgesList=verifyingEdgesList, correctSize=0)     
      TestUtils.checkNoMatch(json=json, sentenceId = sentenceIdForInference2, verifyingEdgesList=verifyingEdgesList, correctSize=0)

      TestUtils.checkMatchedBothSide(json=json, sentenceId = sentenceIdForInference3, verifyingEdgesList=verifyingEdgesList, correctSize=correctSizes(2))
      TestUtils.checkMatchedOneSide(json=json, sentenceId = sentenceIdForInference3, verifyingEdgesList=verifyingEdgesList, correctSize=0)     
      TestUtils.checkNoMatch(json=json, sentenceId = sentenceIdForInference3, verifyingEdgesList=verifyingEdgesList, correctSize=0)

      TestUtils.checkMatchedBothSide(json=json, sentenceId = sentenceIdForInference4, verifyingEdgesList=verifyingEdgesList, correctSize=0)
      TestUtils.checkMatchedOneSide(json=json, sentenceId = sentenceIdForInference4, verifyingEdgesList=verifyingEdgesList, correctSize=0)     
      TestUtils.checkNoMatch(json=json, sentenceId = sentenceIdForInference4, verifyingEdgesList=verifyingEdgesList, correctSize=0)

      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId1, featureId = sentenceId1, sentenceType = SentenceType.CLAIM.index, lang = "ja_JP", SuperiorType.PROPOSITION_ID.index, NonSentenceType.UNSPECIFIED.index, CaseGroupType.UNSPECIFIED.index), transversalState)
      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId2, featureId = sentenceId2, sentenceType = SentenceType.CLAIM.index, lang = "ja_JP", SuperiorType.PROPOSITION_ID.index, NonSentenceType.UNSPECIFIED.index, CaseGroupType.UNSPECIFIED.index), transversalState)
      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId2, featureId = sentenceId3, sentenceType = SentenceType.CLAIM.index, lang = "ja_JP", SuperiorType.PROPOSITION_ID.index, NonSentenceType.UNSPECIFIED.index, CaseGroupType.UNSPECIFIED.index), transversalState)
      deleteFeatureVector(FeatureVectorIdentifier(superiorId = propositionId2, featureId = sentenceId4, sentenceType = SentenceType.CLAIM.index, lang = "ja_JP", SuperiorType.PROPOSITION_ID.index, NonSentenceType.UNSPECIFIED.index, CaseGroupType.UNSPECIFIED.index), transversalState)
    }
  } 

}
