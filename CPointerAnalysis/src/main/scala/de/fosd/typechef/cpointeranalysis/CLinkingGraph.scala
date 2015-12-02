package de.fosd.typechef.cpointeranalysis


import de.fosd.typechef.typesystem.linker.{CSignature, InterfaceWriter}

class CLinkingGraph(interface: CLinking) {

  var mainLocations : List[String] = List() // creates a list of all possible entry points of the used casestudy


  /*
  // import -> export, export -> import
  private def addImports(maps: (HashMap[String, List[String]],HashMap[String, List[String]]),
                        signature: CSignature) : (HashMap[String, List[String]],HashMap[String, List[String]]) = {
    val name = signature.name
    val nameExports = interface.exports.par.filter(_.name.equalsIgnoreCase(name)).toList

    if (nameExports.isEmpty) {
      println("name: " + name)
      importsWithOutExports ::= name
      println("imports: " + signature.pos)
    }

    val impToExp = signature.pos.foldLeft(maps._1)(addPosToMap(_, _, nameExports))
    val expToImp = nameExports.foldLeft(maps._2)((map, export) => export.pos.foldLeft(map)(addPosToMap(_, _, List(signature))))

    (impToExp, expToImp)
  }

  private def addPosToMap(map: HashMap[String, List[String]], pos: Position, add: List[CSignature]) = {
     val file = pos.getFile
     val refs = add.par.flatMap(_.pos.map(_.getFile)).toList
     addReferences(map, file, refs)
  }

  private def addReferences(map: HashMap[String, List[String]], file: String, refs: List[String]) =
    if (map.contains(file)) map.+((file, (map.apply(file) ::: refs).distinct))
    else map.+((file, refs)) */

}
