package purlin.utils

import play.api.libs.json.{JsObject, JsValue, Json}


/** A JSON format IO of a routing strategy.
 *
 * @param routerX      the x index of a router
 * @param routerY      the y index of a router
 * @param srcDirection the source direction
 * @param srcChannel   the source channel
 * @param dstDirection the destination direction
 * @param dstChannel   the destination channel
 */
case class RoutingStrategy(routerX: Option[Int],
                           routerY: Option[Int],
                           srcDirection: Option[Int],
                           srcChannel: Option[Int],
                           dstDirection: Int,
                           dstChannel: Option[Int])

object RoutingStrategy {
  implicit val routingStrategyFormats = Json.format[RoutingStrategy]

  def write(routingStrategy: RoutingStrategy) = {
    Json.toJson(routingStrategy)
  }

  def read(js: JsValue) = {
    val routerX = (js \ "routerX").asOpt[Int]
    val routerY = (js \ "routerY").asOpt[Int]
    val srcDirection = (js \ "srcDirection").asOpt[Int]
    val srcChannel = (js \ "srcChannel").asOpt[Int]
    val dstDirection = (js \ "dstDirection").as[Int]
    val dstChannel = (js \ "routerY").asOpt[Int]
    RoutingStrategy(routerX, routerY, srcDirection, srcChannel, dstDirection, dstChannel)
  }
}


/** A JSON format IO of a message (packet).
 *
 * @param srcX            the x index of source address
 * @param srcY            the y index of source address
 * @param dstX            the x index of destination address
 * @param dstY            the y index of destination address
 * @param injectionCycle  the injection cycle of this message (packet)
 * @param packetLength    the length of this message (packet)
 * @param routingStrategy the routing strategies of this message (packet)
 */
case class Message(srcX: Int,
                   srcY: Int,
                   dstX: Int,
                   dstY: Int,
                   injectionCycle: Option[Int],
                   packetLength: Option[Int],
                   routingStrategy: Option[List[RoutingStrategy]])

object Message {
  implicit val messageFormats = Json.format[Message]

  def write(message: Message) = {
    JsObject(Seq(
      "srcX" -> Json.toJson(message.srcX),
      "srcY" -> Json.toJson(message.srcY),
      "dstX" -> Json.toJson(message.dstX),
      "dstY" -> Json.toJson(message.dstY),
      "injectionCycle" -> Json.toJson(message.injectionCycle),
      "packetLength" -> Json.toJson(message.packetLength),
      "routingStrategy" -> Json.toJson(message.routingStrategy)
    ))
  }

  def read(js: JsValue) = {
    val srcX = (js \ "srcX").as[Int]
    val srcY = (js \ "srcY").as[Int]
    val dstX = (js \ "dstX").as[Int]
    val dstY = (js \ "dstY").as[Int]
    val injectionCycle = (js \ "injectionCycle").asOpt[Int]
    val packetLength = (js \ "packetLength").asOpt[Int]
    val routingStrategy = (js \ "routingStrategy").asOpt[List[RoutingStrategy]]
    Message(srcX, srcY, dstX, dstY, injectionCycle, packetLength, routingStrategy)
  }
}

case class GlobalRouting(messages: List[Message])

object GlobalRouting {
  def write(globalRouting: GlobalRouting) = {
    JsObject(Seq(
      "messages" -> Json.toJson(globalRouting.messages)
    ))
  }

  def read(js: JsValue) = {
    val messages = (js \ "messages").as[List[Message]]
    GlobalRouting(messages)
  }
}



