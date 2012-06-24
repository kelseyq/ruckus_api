package com.kelseyinnis.ruckus

import org.scalatra._
import org.scalatra.ActionResult
import net.liftweb.json._
import net.liftweb.json.JsonDSL._
import net.liftweb.json.Serialization.{read, write}
import com.mongodb.casbah.Imports._
import com.mongodb.casbah.MongoURI
import scala.util.Properties

class RuckusServlet extends ScalatraServlet  {

val collName = "ruckus_data" 
val MongoSetting(db) = Properties.envOrNone("MONGOHQ_URL")
val mongoColl: MongoCollection = db(collName)

implicit val formats = DefaultFormats


  get("/") {
    <html>
      <body>
        <h1>Hello, world!</h1>
      </body>
    </html>
  }
  
  case class Reaction(user_id: String, reaction_type: String, content: String)

  get("/mlb/:date/:team/gameinfo") {
    //todo: implement
      Ok()
  }

  post("/mlb/:date/:team/reaction/") {
      val theReaction = parse(request.body).extract[Reaction]

      if (validateReaction(theReaction)) {
        val builder = MongoDBObject.newBuilder
        builder += "user_id" -> theReaction.user_id
        builder += "game_date" -> params("date")
        builder += "team" -> params("team")
        builder += "inning" -> "0t"
        builder += "reaction_type" -> theReaction.reaction_type
        builder += "content" -> theReaction.content
        builder += "upvotes" -> 0
        builder += "upvoters" -> MongoDBList.newBuilder.result
        builder += "downvotes" -> 0
        builder += "downvoters" -> MongoDBList.newBuilder.result
        builder += "flags" -> 0
        builder += "flaggers" -> MongoDBList.newBuilder.result
        val newReaction = builder.result
        mongoColl += newReaction
      }

      val otherReactions = getReactions(theReaction.user_id, params("date"), params("team"))

      if (!otherReactions.isEmpty) {
          val json =
            ("reaction1" -> getReactionJson(otherReactions(0))) ~
            ("reaction2" -> getReactionJson(otherReactions(1))) ~
            ("reaction2" -> getReactionJson(otherReactions(2)))
          pretty(render(json))
      } else Ok()
  }

  get("/mlb/:date/:team/lurk") {
      val otherReactions = getReactions(params("user_id"), params("date"), params("team"))

      if (!otherReactions.isEmpty) {
          val json =
            ("reaction1" -> getReactionJson(otherReactions(0))) ~
            ("reaction2" -> getReactionJson(otherReactions(1))) ~
            ("reaction2" -> getReactionJson(otherReactions(2)))
          pretty(render(json))
      } else Ok()
  }

  private def validateReaction(reaction: Reaction): Boolean = {
    !(reaction.content.isEmpty)
  }

  private def getReactionJson(dbObj: MongoDBObject) = {
                (("url" -> ("/mlb/" + params("date") + "/" + params("team") + "/reaction/" + (dbObj.getAs[String]("_id") getOrElse("00000")))) ~ 
                ("reaction_id" -> (dbObj.getAs[ObjectId]("_id").map(_.toString) getOrElse("00000"))) ~ 
                ("reaction_type" -> (dbObj.getAs[String]("reaction_type") getOrElse("string"))) ~
           //  ("user_id" -> (dbObj.getAs[String]("user_id") getOrElse("no id"))) ~
                ("content" -> (dbObj.getAs[String]("content") getOrElse("00000")))) 
  }

  private def getReactions(reacting_user: String, date: String, team: String): List[MongoDBObject] = {
      //hideously inefficient--puttin the "hack" in "hackathon"
      //TODO: filter out flagged reactions & reactions with too many downvotes
      //      bubble up higher voted reactions? 

      val filter = ("user_id" $ne 0) //("user_id" $ne reacting_user)
      //TODO: filter by game

      val otherReactions = mongoColl.find(filter)
      val totalReactions = otherReactions.count.toInt
      if (totalReactions > 2) {
        val random1 = scala.util.Random.nextInt(totalReactions)
        val random2 = scala.util.Random.nextInt(totalReactions)
        val random3 = scala.util.Random.nextInt(totalReactions)

        val reaction1 = mongoColl.find(filter).limit(-1).skip(random1).next()
        val reaction2 = mongoColl.find(filter).limit(-1).skip(random2).next()
         val reaction3 = mongoColl.find(filter).limit(-1).skip(random3).next()
        List(reaction1, reaction2, reaction3)
      } else {
        List()
      }
  }
  
  get("/mlb/:date/:team/reaction/:reaction_id") {
    val o : DBObject = MongoDBObject("_id" -> new ObjectId(params("reaction_id")))
    val u = mongoColl.findOne(o)
    pretty(render(u.map(getReactionJson(_)).getOrElse("")))
  }

  post("/admin/clear/") {
    mongoColl.dropCollection
    Ok()
  }

  //todo: validate users, make sure they can only vote once

  post("/mlb/:date/:team/:reaction_id/upvote") {
    val oid : DBObject = MongoDBObject("_id" -> new ObjectId(params("reaction_id")))
    mongoColl.update(oid, $push("upvoters" -> params.getOrElse("user_id", halt(400))))
    mongoColl.update(oid, $inc("upvotes" -> params.getOrElse("intensity","1").toInt))
    Ok()
  }
  
  post("/mlb/:date/:team/:reaction_id/downvote") {
    val oid : DBObject = MongoDBObject("_id" -> new ObjectId(params("reaction_id")))
    mongoColl.update(oid, $push("downvoters" -> params.getOrElse("user_id", halt(400))))
    mongoColl.update(oid, $inc("downvotes" -> params.getOrElse("intensity","1").toInt))
    Ok()
  }
  
  post("/mlb/:date/:team/:reaction_id/flag") {
    val oid : DBObject = MongoDBObject("_id" -> new ObjectId(params("reaction_id")))
    mongoColl.update(oid, $push("flaggers" -> params("user_id")))
    mongoColl.update(oid, $inc("flags" -> 1))
    Ok()
  }

  get("/mlb/:date/:team/reactions/") {
    Ok()
  }

  get("/mlb/:date/:team/top") {
    Ok()
  }

}

object MongoSetting {
  def unapply(url: Option[String]): Option[MongoDB] = {
    val regex = """mongodb://(\w+):(\w+)@([\w|\.]+):(\d+)/(\w+)""".r
    url match {
      case Some(regex(u, p, host, port, dbName)) =>
        val db = MongoConnection(host, port.toInt)(dbName)
        db.authenticate(u,p)
        Some(db)
      case None =>
        Some(MongoConnection("localhost", 27017)("ruckus_test"))
    }
  }
}