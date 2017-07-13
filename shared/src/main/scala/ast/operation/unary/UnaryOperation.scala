package maml.ast.operation.unary

import maml.ast.operation.Operation


/** Operations which should only have one argument. */
abstract class UnaryOperation(override val symbol: String) extends Operation(symbol)
    with Product with Serializable

