import lista5.Lista5

val x = new Lista5();

val tree = x.generateTree(1,10)
x.multiplyElements(tree)
x.deleteDuplicatesDepth(tree)
x.deleteDuplicatesBreadth(tree)

val tree2 = x.generateTree(2,4)
x.deleteDuplicatesDepth(tree2)
x.deleteDuplicatesBreadth(tree2)
