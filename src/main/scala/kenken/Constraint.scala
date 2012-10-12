package kenken

abstract class Constraint {
}

abstract class Line extends Constraint {
}

abstract class BoxConstraint extends Constraint

class SingleCellBox extends BoxConstraint {
}

abstract class MultiCellBox extends Constraint

class AddBox extends MultiCellBox {
}

class MultiplyBox extends MultiCellBox

class SubtractBox extends MultiCellBox

class DivideBox extends MultiCellBox
