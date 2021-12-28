package tetriski.purlin.utils

/** A mesh network model.
 *
 * TODO: combine packet-switched and circuit-switched network models.
 */
class MeshModel {
  var channelSize = 0
  var xSize = 0
  var ySize = 0

  /** Create this mesh network model.
   *
   * @param channelSize the number of channels
   * @param xSize       the x size of this network
   * @param ySize       the y size of this network
   *
   */
  def this(channelSize: Int, xSize: Int, ySize: Int) {
    this()
    this.channelSize = channelSize
    this.xSize = xSize
    this.ySize = ySize
  }
}
