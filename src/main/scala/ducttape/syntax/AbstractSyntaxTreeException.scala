// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.syntax

import AST.Node

class AbstractSyntaxTreeException(element: Node, msg: String)
  extends RuntimeException(s"ERROR: line ${element.endPos.line} column ${element.endPos.column}: $msg")
