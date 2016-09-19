package de.fosd.typechef.spllift

import de.fosd.typechef.options.FrontendOptionsWithConfigFiles

object Launch extends App {

    override def main(args: Array[String]): Unit = {
        val opt = new FrontendOptionsWithConfigFiles
        
        val spllift = new CSPLliftFrontend(null)
    }

}

