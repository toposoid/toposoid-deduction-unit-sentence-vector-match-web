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

import com.ideal.linked.common.DeploymentConverter.conf
import com.ideal.linked.toposoid.common.{TRANSVERSAL_STATE, ToposoidUtils, TransversalState}
import com.ideal.linked.toposoid.knowledgebase.featurevector.model.{FeatureVectorIdentifier, FeatureVectorSearchResult, SingleFeatureVectorForSearch}
import com.ideal.linked.toposoid.knowledgebase.model.{KnowledgeBaseEdge, KnowledgeBaseNode}
import com.ideal.linked.toposoid.knowledgebase.regist.model.Knowledge
import com.ideal.linked.toposoid.protocol.model.base.{AnalyzedSentenceObject, AnalyzedSentenceObjects, CoveredPropositionEdge, CoveredPropositionNode, KnowledgeBaseSideInfo, MatchedFeatureInfo}
import com.ideal.linked.toposoid.protocol.model.neo4j.{Neo4jRecordMap, Neo4jRecords}
import com.ideal.linked.toposoid.vectorizer.FeatureVectorizer
import com.typesafe.scalalogging.LazyLogging

import javax.inject._
import play.api._
import play.api.libs.json.{Json, __}
import play.api.mvc._
import play.api.libs.json.JsValue

import scala.util.{Failure, Success, Try}
import com.ideal.linked.toposoid.protocol.model.base.VerifyingEdges
import com.ideal.linked.toposoid.common.SentenceType
import com.ideal.linked.toposoid.common.DeductionUtils
import com.ideal.linked.toposoid.common.ScopeType
import com.ideal.linked.toposoid.common.FeatureType
import com.ideal.linked.toposoid.common.Neo4JUtilsImpl
import com.ideal.linked.toposoid.protocol.model.base.DeductionResult
import com.ideal.linked.toposoid.protocol.model.base.MatchedKnowledgeNode
case class FeatureVectorSearchInfo(propositionId:String, sentenceId:String, sentenceType:Int, lang:String, featureId:String, similarity:Float)

/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class HomeController @Inject()(val controllerComponents: ControllerComponents) extends BaseController /*with DeductionUnitControllerForSemiGlobal*/ with LazyLogging {

  def execute():Action[JsValue] = Action(parse.json[JsValue])  { request =>
    val transversalState = Json.parse(request.headers.get(TRANSVERSAL_STATE .str).get).as[TransversalState]
    try {
      val json = request.body
      val analyzedSentenceObjects: AnalyzedSentenceObjects = Json.parse(json.toString).as[AnalyzedSentenceObjects]
      val asos: List[AnalyzedSentenceObject] = analyzedSentenceObjects.analyzedSentenceObjects

      val result:List[VerifyingEdges] = asos.foldLeft(List.empty[VerifyingEdges]){
        (acc, aso) => {    
          acc :+ VerifyingEdges(            
            propositionId = aso.knowledgeBaseSemiGlobalNode.propositionId,
            sentenceId = aso.knowledgeBaseSemiGlobalNode.sentenceId,
            coveredPropositionEdges = analyzeGraphKnowledgeForSemiGlobal(aso, transversalState)
          )
        }
      }
      logger.info(ToposoidUtils.formatMessageForLogger("Basic edge analysis completed.", transversalState.userId))      
      Ok(Json.toJson(result)).as(JSON)      
    }catch {
      case e: Exception => {
        logger.error(ToposoidUtils.formatMessageForLogger(e.toString, transversalState.userId), e)
        BadRequest(Json.obj("status" -> "Error", "message" -> e.toString()))
      }
    }
  }

  private def analyzeGraphKnowledgeForSemiGlobal(aso: AnalyzedSentenceObject, transversalState:TransversalState): List[CoveredPropositionEdge] = {
    getMatchedSentenceFeature(aso ,
      transversalState)
  }

  private def getMatchedSentenceFeature(aso:AnalyzedSentenceObject, transversalState:TransversalState): List[CoveredPropositionEdge] = {

    val originalSentenceId = aso.knowledgeBaseSemiGlobalNode.sentenceId
    val originalSentenceType = aso.knowledgeBaseSemiGlobalNode.sentenceType
    val sentence = aso.knowledgeBaseSemiGlobalNode.sentence
    val lang = aso.knowledgeBaseSemiGlobalNode.localContextForFeature.lang

    val vector = FeatureVectorizer.getSentenceVector(Knowledge(sentence, lang, "{}"), transversalState)
    val json: String = Json.toJson(SingleFeatureVectorForSearch(vector = vector.vector, num = conf.getString("TOPOSOID_SENTENCE_VECTORDB_SEARCH_NUM_MAX").toInt)).toString()
    val featureVectorSearchResultJson: String = ToposoidUtils.callComponent(json, conf.getString("TOPOSOID_SENTENCE_VECTORDB_ACCESSOR_HOST"), conf.getString("TOPOSOID_SENTENCE_VECTORDB_ACCESSOR_PORT"), "search", transversalState)
    val result = Json.parse(featureVectorSearchResultJson).as[FeatureVectorSearchResult]

    //VecotrDBにClaimとして存在している場合に推論が可能になる
    val (ids, similarities) = (result.ids zip result.similarities).foldLeft((List.empty[FeatureVectorIdentifier], List.empty[Float])) {
      (acc, x) => {
        x._1.sentenceType match {
          case SentenceType.CLAIM.index => (acc._1 :+ x._1, acc._2 :+ x._2)
          case _ => acc
        }
      }
    }

    val filteredResult = FeatureVectorSearchResult(ids, similarities, result.statusInfo) 
    val deductionUnitName = conf.getString("TOPOSOID_DEDUCTION_UNIT_NAME")
    filteredResult.ids.size match {
      case 0 => List.empty[CoveredPropositionEdge]
      case _ => {        
        val featureVectorSearchInfoList = extractExistInNeo4JResultForSentence(filteredResult, originalSentenceType, transversalState)        
        val matchedKnowledgeNodes = featureVectorSearchInfoList.map(x => {
          MatchedKnowledgeNode(
            propositionId = x.propositionId,
            sentenceId = x.sentenceId,
            nodeId = "",
            caseNameOnEdge = "",
            isDenialWord = false,
            nodeType = x.sentenceType,
            featureInfo = MatchedFeatureInfo(featureId = x.featureId, similarity = x.similarity)
          )          
        })

        aso.edgeList.map(x => {
          val sourceNode = aso.nodeMap.get(x.sourceId).get.asInstanceOf[KnowledgeBaseNode]
          val destinationNode = aso.nodeMap.get(x.destinationId).get.asInstanceOf[KnowledgeBaseNode]
          val sourceCoveredPropositionNode = CoveredPropositionNode(
            terminalId = sourceNode.nodeId,
            terminalSurface = sourceNode.predicateArgumentStructure.surface,
            terminalUrl = "",
            matchedKnowledgeNodes = matchedKnowledgeNodes,
            isConfirmed = true,
            deductionUnit = deductionUnitName
          )

          val destinationCoveredPropositionNode = CoveredPropositionNode(
            terminalId = destinationNode.nodeId,
            terminalSurface = destinationNode.predicateArgumentStructure.surface,
            terminalUrl = "",
            matchedKnowledgeNodes = matchedKnowledgeNodes,
            isConfirmed = true,
            deductionUnit = deductionUnitName
          )
          CoveredPropositionEdge(sourceCoveredPropositionNode, destinationCoveredPropositionNode)
        }) 
      }
    }    
        
  }

  private def extractExistInNeo4JResultForSentence(featureVectorSearchResult: FeatureVectorSearchResult, originalSentenceType: Int, transversalState:TransversalState): List[FeatureVectorSearchInfo] = {

    val neo4jUtils = Neo4JUtilsImpl()
    (featureVectorSearchResult.ids zip featureVectorSearchResult.similarities).foldLeft(List.empty[FeatureVectorSearchInfo]) {
      (acc, x) => {
        val idInfo = x._1
        val propositionId = idInfo.superiorId
        val lang = idInfo.lang
        val featureId = idInfo.featureId
        val similarity = x._2
        val nodeType: String = ToposoidUtils.getNodeType(idInfo.sentenceType, ScopeType.SEMIGLOBAL.index, FeatureType.SENTENCE.index)
        //Check whether featureVectorSearchResult information exists in Neo4J
        val query = "MATCH (n:%s) WHERE n.propositionId='%s' AND n.sentenceId='%s' RETURN n".format(nodeType, propositionId, featureId)
        val jsonStr: String = neo4jUtils.getCypherQueryResult(query, "", transversalState)
        val neo4jRecords: Neo4jRecords = Json.parse(jsonStr).as[Neo4jRecords]
        neo4jRecords.records.size match {
          case 0 => acc
          case _ => {
            val idInfoOnNeo4jSide = neo4jRecords.records.head.head.value.semiGlobalNode.get
            //sentenceType returns the originalSentenceType of the argument
            acc :+ FeatureVectorSearchInfo(idInfoOnNeo4jSide.propositionId, idInfoOnNeo4jSide.sentenceId, originalSentenceType, lang, featureId, similarity)
          }
        }
      }
    }
  }

}