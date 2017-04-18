package com.containant.casestudies

/** In this experiment, we create syntax highlighting color schemes
 *  based on specific target constraints, maximizing an aesthetic
 *  (pragmatic) fitness function.
 */

import java.awt.Color
import com.containant._
import com.containant.heuristics._

object CS3SyntaxHigh {
  //////////////////////////////////////////////////////////////////////
  // Configuration
  val _seed: Int = 0xDEADBEEF
  val _runs: Int = 25
  
  object Hmma extends AntHeuristic {
    override val _maxPheromone: Double = 40
    override val _evaporationRate: Double = 0.4
    override val _iterations: Int = 1000
    override val _antNumber: Int = 1
    override val _minimumFraction: Double = 0.10
    override val _recursionDepth: Int = 5
    override val RNG: java.util.Random = new java.util.Random(_seed)
    override def toString: String = "mma"
  } 
  object Hgre extends GrEvoHeuristic {
    override val _population: Int = 25
    override val _length: Int = 9
    override val _maxChoice: Int = 25
    override val _tournamentSize = 5
    override val _generations = 40
    override val _recursionDepth = 5
    override val RNG: java.util.Random = new java.util.Random(_seed)
    override def toString: String = "gre"
  }

  object Hran extends RandomHeuristic {
    override val _iterations = 1000
    override val _recursionDepth = 5
    override val RNG: java.util.Random = new java.util.Random(_seed)
    override def toString: String = "ran"
  }

  //////////////////////////////////////////////////////////////////////
  // Problem Description

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
    
    /** A pastelized variant of the given color (averaged with white) */
    def pastelized: RGB = {
      val ir: Int = (this.red + 255)/2
      val ig: Int = (this.green + 255)/2
      val ib: Int = (this.blue + 255)/2
      RGB(ir,ig,ib)
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
    
    def toCSS: String = 
    s"""    /* Aspects. */
    .Comment       { color: $cString }
    .Keyword       { color: $cKeyword }
    .String        { color: $cString }
    .Number        { color: $cModule }
    .Symbol        { color: $cSymbol }
    .PrimitiveType { color: $cPrimitive }
    .Operator      {}

    /* NameKinds. */
    .Bound                  { color: $cBound }
    .InductiveConstructor   { color: $cConstructor }
    .CoinductiveConstructor { color: $cConstructor }
    .Datatype               { color: $cPrimitive }
    .Field                  { color: $cField }
    .Function               { color: $cPrimitive }
    .Module                 { color: $cModule }
    .Postulate              { color: $cPrimitive }
    .Primitive              { color: $cPrimitive }
    .Record                 { color: $cPrimitive }

    /* OtherAspects. */
    .DottedPattern      {}
    .UnsolvedMeta       { color: black; background: yellow         }
    .UnsolvedConstraint { color: black; background: yellow         }
    .TerminationProblem { color: black; background: #FFA07A        }
    .IncompletePattern  { color: black; background: #F5DEB3        }
    .Error              { color: red;   text-decoration: underline }
    .TypeChecks         { color: black; background: #ADD8E6        }

    /* Standard attributes. */
    a { text-decoration: none }
    a[href]:hover { text-decoration: underline }
    body { background: $cBackground }
    """
  }
  
  /** An abstract module for creating schemes for different backgrounds.
   */
  class ColorModule(targetBackground: RGB) extends Module {
    val i0: Int = 0
    val i10: Int = 10
    val i20: Int = 20
    val i30: Int = 30
    val i40: Int = 40
    val i50: Int = 50
    val i60: Int = 60
    val i70: Int = 70
    val i80: Int = 80
    val i90: Int = 90
    val i100: Int = 100
    val i110: Int = 110
    val i120: Int = 120
    val i130: Int = 130
    val i140: Int = 140
    val i150: Int = 150
    val i160: Int = 160
    val i170: Int = 170
    val i180: Int = 180
    val i190: Int = 190
    val i200: Int = 200
    val i210: Int = 210
    val i220: Int = 220
    val i230: Int = 230
    val i240: Int = 240
    val i250: Int = 250
    
    def pastel(red: Int, green: Int, blue: Int): RGB = 
      RGB(red,green,blue).pastelized
      
    def mkScheme(
      cString: RGB,
      cKeyword: RGB,
      cModule: RGB,
      cSymbol: RGB,
      cPrimitive: RGB,
      cBound: RGB,
      cConstructor: RGB,
      cField: RGB
    ): Scheme = Scheme(
        cString,
        cKeyword,
        cModule,
        cSymbol,
        cPrimitive,
        cBound,
        cConstructor,
        cField,
        targetBackground
    )
  }
  
  object DarkBlueColorModule extends ColorModule( RGB(0,43,54) )
  object YellowColorModule extends ColorModule( RGB(255,255,204) )
 
  //////////////////////////////////////////////////////////////////////
  // Experiment Details


  def main(args: Array[String]): Unit = {
    import com.containant.casestudies.Framework
    println("\n---------------------------------")
    println("Case Study 3: Syntax Highlighting (Dark Blue)")
    println("Runs: " + _runs)
    
    val comparisonDB =
      Framework.experiment[Scheme](Hmma, Hgre, _runs, DarkBlueColorModule, _.readability)

    val referenceDB =
      Framework.experiment[Scheme](Hran, Hran, _runs, DarkBlueColorModule, _.readability)
    
    println("heuristic,min,mean,max,var")
    println(comparisonDB.summary1)
    println(comparisonDB.summary2)
    println(referenceDB.summary1)
    println("p: " + comparisonDB.pvalue)
    println()

    println("\n---------------------------------")
    println("Case Study 3: Syntax Highlighting (Yellow)")
    println("Runs: " + _runs)
    
    val comparisonY =
      Framework.experiment[Scheme](Hmma, Hgre, _runs, YellowColorModule, _.readability)

    val referenceY =
      Framework.experiment[Scheme](Hran, Hran, _runs, YellowColorModule, _.readability)
    
    println("heuristic,min,mean,max,var")
    println(comparisonY.summary1)
    println(comparisonY.summary2)
    println(referenceY.summary1)
    println("p: " + comparisonY.pvalue)
    println()
  }
  
}
