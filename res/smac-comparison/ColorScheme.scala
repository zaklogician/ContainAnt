package com.containant.casestudies

import java.awt.Color

// the color scheme program used by the SMAC verification

object ColorScheme {

  case class RGB(red: Int, green: Int, blue: Int) {
    override def toString: String = s"RGB($red,$green,$blue)"
    
    /** The normalized sRGB distance to the given RGB color. */
    def deltaE(that: RGB): Double = {
      val ir: Double = this.red*1.0 / 255.0
      val ig: Double = this.green*1.0 / 255.0
      val ib: Double = this.blue*1.0 / 255.0
      val ar: Double = that.red*1.0 / 255.0
      val ag: Double = that.green*1.0 / 255.0
      val ab: Double = that.blue*1.0 / 255.0
      val quad = (ir - ar)*(ir - ar) + (ig - ag)*(ig - ag) + (ib - ab)*(ib - ab)
      Math.sqrt(quad/3)
    }
    
    /** The normalized angular (color wheel) distance to the given RGB color.*/
    def deltaH(that: RGB): Double = {
      val thisHSB: Array[Float] = Array.ofDim(3)
      Color.RGBtoHSB(this.red, this.green, this.blue, thisHSB)
      val thisHue = thisHSB(0)
      val thatHSB: Array[Float] = Array.ofDim(3)
      Color.RGBtoHSB(that.red, that.green, that.blue, thatHSB)
      val thatHue = thatHSB(0)
      val difference = thisHue - thatHue
      val result = if (difference > 0) difference else difference + 1
      result
    }
  }
  
  /** An Agda syntax highlighting color scheme.*/
  case class Scheme(
    cString: RGB,
    cKeyword: RGB,
    cModule: RGB,
    cSymbol: RGB,
    cPrimitive: RGB,
    cBound: RGB,
    cConstructor: RGB,
    cField: RGB,
    cBackground: RGB
  ) {
    
    /* The list of all foreground colors. */
    private val foreground: List[RGB] = List(
      cString,
      cKeyword,
      cModule,
      cSymbol,
      cPrimitive,
      cBound,
      cConstructor,
      cField
    )
    
    /** The readability score of the color scheme:
     *  the sum of the sRGB distances from the backgrounds
     *  plus 2x1 point for each complementary color pair.
     */
    val readability: Double = {
      val background = for( x <- foreground ) yield (x deltaE cBackground)
      val linkBackground = List(0) //for( x <- foreground ) yield (x deltaE cLinkBackground)
      val harmony = for( x <- foreground; y <- foreground ) yield {
        val delta = x deltaH y
        if (0.4 < delta && delta < 0.6) 1 else 0
      }
      background.sum + linkBackground.sum + harmony.sum
    }
  }
  
  //object DarkBlueColorModule extends ColorModule( RGB(0,43,54) )
  //object YellowColorModule extends ColorModule( RGB(255,255,204) )
  
  //////////////////////////////////////////////////////////////////////
  // Boilerplate
  
  def parseInt(str: String): Int = {
    val x = str.dropWhile(_ == '\'').takeWhile(_ != '\'').toInt
    x
  }
  
  var cStringR: Int = 0
  var cStringG: Int = 0
  var cStringB: Int = 0
  var cKeywordR: Int = 0
  var cKeywordG: Int = 0
  var cKeywordB: Int = 0
  var cModuleR: Int = 0
  var cModuleG: Int = 0
  var cModuleB: Int = 0
  var cSymbolR: Int = 0
  var cSymbolG: Int = 0
  var cSymbolB: Int = 0
  var cPrimitiveR: Int = 0
  var cPrimitiveG: Int = 0
  var cPrimitiveB: Int = 0
  var cBoundR: Int = 0
  var cBoundG: Int = 0
  var cBoundB: Int = 0
  var cConstructorR: Int = 0
  var cConstructorG: Int = 0
  var cConstructorB: Int = 0
  var cFieldR: Int = 0
  var cFieldG: Int = 0
  var cFieldB: Int = 0

  def cString: RGB = RGB(cStringR,cStringG,cStringB)
  def cKeyword: RGB = RGB(cKeywordR,cKeywordG,cKeywordB)
  def cModule: RGB = RGB(cModuleR,cModuleG,cModuleB)
  def cSymbol: RGB = RGB(cSymbolR,cSymbolG,cSymbolB)
  def cPrimitive: RGB = RGB(cPrimitiveR,cPrimitiveG,cPrimitiveB)
  def cBound: RGB = RGB(cBoundR,cBoundG,cBoundB)
  def cConstructor: RGB = RGB(cConstructorR,cConstructorG,cConstructorB)
  def cField: RGB = RGB(cFieldR,cFieldG,cFieldB)
  def cBackground: RGB = RGB(0,43,54)

  
  def parseArgs(list: List[String]): Unit = list match {
    case "-cStringR" :: value :: rest => {cStringR = parseInt(value); parseArgs(rest) }
    case "-cStringG" :: value :: rest => {cStringG = parseInt(value); parseArgs(rest) }
    case "-cStringB" :: value :: rest => {cStringB = parseInt(value); parseArgs(rest) }
    case "-cKeywordR" :: value :: rest => {cKeywordR = parseInt(value); parseArgs(rest) }
    case "-cKeywordG" :: value :: rest => {cKeywordG = parseInt(value); parseArgs(rest) }
    case "-cKeywordB" :: value :: rest => {cKeywordB = parseInt(value); parseArgs(rest) }
    case "-cModuleR" :: value :: rest => {cModuleR = parseInt(value); parseArgs(rest) }
    case "-cModuleG" :: value :: rest => {cModuleG = parseInt(value); parseArgs(rest) }
    case "-cModuleB" :: value :: rest => {cModuleB = parseInt(value); parseArgs(rest) }
    case "-cSymbolR" :: value :: rest => {cSymbolR = parseInt(value); parseArgs(rest) }
    case "-cSymbolG" :: value :: rest => {cSymbolG = parseInt(value); parseArgs(rest) }
    case "-cSymbolB" :: value :: rest => {cSymbolB = parseInt(value); parseArgs(rest) }
    case "-cPrimitiveR" :: value :: rest => {cPrimitiveR = parseInt(value); parseArgs(rest) }
    case "-cPrimitiveG" :: value :: rest => {cPrimitiveG = parseInt(value); parseArgs(rest) }
    case "-cPrimitiveB" :: value :: rest => {cPrimitiveB = parseInt(value); parseArgs(rest) }
    case "-cBoundR" :: value :: rest => {cBoundR = parseInt(value); parseArgs(rest) }
    case "-cBoundG" :: value :: rest => {cBoundG = parseInt(value); parseArgs(rest) }
    case "-cBoundB" :: value :: rest => {cBoundB = parseInt(value); parseArgs(rest) }
    case "-cConstructorR" :: value :: rest => {cConstructorR = parseInt(value); parseArgs(rest) }
    case "-cConstructorG" :: value :: rest => {cConstructorG = parseInt(value); parseArgs(rest) }
    case "-cConstructorB" :: value :: rest => {cConstructorB = parseInt(value); parseArgs(rest) }
    case "-cFieldR" :: value :: rest => {cFieldR = parseInt(value); parseArgs(rest) }
    case "-cFieldG" :: value :: rest => {cFieldG = parseInt(value); parseArgs(rest) }
    case "-cFieldB" :: value :: rest => {cFieldB = parseInt(value); parseArgs(rest) }
    case unknown :: rest => parseArgs(rest)
    case Nil => { Unit }
  }
  
  def main(args: Array[String]): Unit = {
    println(args mkString ",")
    parseArgs(args.toList)
    val quality: Double = Scheme(
      cString,
      cKeyword,
      cModule,
      cSymbol,
      cPrimitive,
      cBound,
      cConstructor,
      cField,
      cBackground
    ).readability
    println("Result of algorithm run: SUCCESS, 0, 0, " + (1/quality) + ", 0")
  }
  
}
