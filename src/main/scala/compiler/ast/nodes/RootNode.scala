package compiler.ast.nodes

import compiler.ast.NodeBase

class RootNode extends NodeBase("") {
  
  override def setText(text: String) = {
    sys.error("Root cannot accept a text value")
  }
}
