package de.fosd.typechef.customization.clinking

import java.io.{File, FileWriter}

import de.fosd.typechef.error.Position
import de.fosd.typechef.featureexpr.FeatureExprParser
import de.fosd.typechef.typesystem.CType
import de.fosd.typechef.typesystem.linker._

trait CInterfaceWriter extends InterfaceWriter {

  def writeExportInterface(interface: CInterface, file: File) {
    val stream = new FileWriter(file)
    scala.xml.XML.write(stream, exportInterfaceToXML(interface), "UTF-8", true, null)
    stream.close()
  }

  def readMergedInterfaceFromXML(node: scala.xml.Node) = new CInterface(
    null,
    (node \ "feature").map(_.text.trim).toSet,
    (node \ "newfeature").map(_.text.trim).toSet,
    (node \ "import").map(signatureFromXML),
    (node \ "export").map(signatureFromXML)
  )

  def exportInterfaceToXML(int: CInterface): xml.Elem =
    <interface>
      <featuremodel>
        {int.featureModel.toTextExpr}
      </featuremodel>{int.importedFeatures.map(x => <feature>
      {x}
    </feature>)}{int.declaredFeatures.map(x => <newfeature>
      {x}
    </newfeature>)}{int.exports.map(x => <export>
      {signatureToXML(x)}
    </export>)}
    </interface>

  private def signatureFromXML(node: scala.xml.Node): CSignature = {
    val sig = node \ "sig"
    CSignature(
      (sig \ "name").text.trim,
      CType.fromXML(sig \ "type"),
      new FeatureExprParser().parse((sig \ "featureexpr").text),
      (sig \ "pos").map(positionFromXML),
      (sig \ "extraFlag").flatMap(extraFlagFromXML).filter(_.isDefined).map(_.get).toSet
    )
  }
  private def positionFromXML(node: scala.xml.Node): Position = {
    val col = (node \ "col").text.trim.toInt
    val line = (node \ "line").text.trim.toInt
    val file = (node \ "file").text.trim
    new Position() {
      def getColumn: Int = col
      def getLine: Int = line
      def getFile: String = file
    }
  }
  private def extraFlagFromXML(node: scala.xml.Node): Seq[Option[CFlag]] = {
    (node \ "@name").map(n => if (n.text == "WeakExport") Some(WeakExport) else None)
  }
  private def posToXML(p: Position) =
    <pos>
      <file>
        {p.getFile}
      </file>
      <line>
        {p.getLine}
      </line>
      <col>
        {p.getColumn}
      </col>
    </pos>
}
