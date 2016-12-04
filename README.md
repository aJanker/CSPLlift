CSPLlift
========

Implementation of a connector to the IFDS solver [Heros](https://github.com/Sable/heros/) for configurable software systems in C.
The engine relies on the variability-aware parsing and analysis infrastructure [TypeChef](https://ckaestne.github.io/TypeChef/).


Installation and Usage
----------------------

CSPLlift currently requires a modified version of TypeChef. This version will be automatically installed and can be found [here](https://github.com/aJanker/TypeChef).
To install the last version of CSPLlift simply run:

    git clone https://github.com/aJanker/CSPLlift.git
    cd CSPLlift
    ./make.sh

The commands create a run-script (lift.sh) for the project. CSPLlift comes with a reference data-flow analysis (parameter: --spllift taint) and a command line interface for evaluation of the data-flow analysis. To apply CSPLlift in a real setting, a proper project setup has to be passed to the engine. Since the project setup of existing software systems is difficult to integrate with CSPLlift, we currently support only two systems. The systems, including the proper setup to run with CSPLlift, are available on github, too: [mbedTLS](https://github.com/aJanker/CSPLlift-mbedTLS-Analysis), and [OpenSSL](https://github.com/aJanker/CSPLlift-OpenSSL-Analysis).

License
-------

CSPLlift is published as open source under LGPL 3.0. See [LICENSE](LICENSE.md).
