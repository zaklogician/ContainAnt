package com.containant


class ModuleGrammar(module: Module) extends LBNF {
  import java.lang.reflect.Method
  override type Sort = Class[_]
  override type Label = Method
  
  private val providers: List[Method] =
    module.getClass.getMethods.toList.sortBy(_.getName).filter { m => 
      !List( "equals","hashCode","toString", "getClass",
            "wait", "notify", "notifyAll"
           ).contains(m.getName)
    }
  
  override def sort(label: Label): Sort =
    label.getReturnType
  
  override val sorts: Seq[Sort] = 
    providers.map(sort).distinct
  
  override def labels(sort: Sort): Seq[Label] =
    providers.filter { p => sort.isAssignableFrom(p.getReturnType) }
  
  override def rule(label: Label): Seq[Sort] = 
    label.getParameterTypes
    
    
  def construct(tree: SyntaxTree): Object = {
    val arguments = tree.subtrees.map(construct)
    tree.label.invoke(module, arguments:_*)
  }
}
