package de.fosd.typechef.cmodule.ccallgraph.clinking

import java.io.File

import de.fosd.typechef.cmodule.{CModuleCommons, CModuleNamings}

trait CModuleLinkCommons extends CModuleCommons with CModuleNamings {

    def getCModuleLinkMapPath(dir: String, fileExtension: String): String =
        dir + File.separatorChar + MODULE_DIR_NAME + File.separatorChar + MODULE_INTERFACE_FILENAME + fileExtension
}
