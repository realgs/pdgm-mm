/////////////////////////////////////////////////////////////////////////////////
// 1                                                                           //
/////////////////////////////////////////////////////////////////////////////////
sealed trait tree[+A]
case object Empty extends tree[Nothing]
case class Node[+A](value: A, children: List[tree[A]]) extends tree[A]

def create_full_tree (general_degree: Int, depth: Int): tree[Int] =
{
    if(depth == 0) Empty
    else
    {
        def append_children(degree: Int, children: List[tree[Int]]): List[tree[Int]] =
        {
            if(degree == 0) children
            else append_children(degree - 1, create_full_tree(general_degree, depth - 1)::children)
        }
        val random = scala.util.Random
        Node[Int](random.nextInt(10), append_children(general_degree, Nil))
    }
}
val int_tree = create_full_tree(2, 3)

/////////////////////////////////////////////////////////////////////////////////
// 2                                                                           //
/////////////////////////////////////////////////////////////////////////////////
def multiply(original_tree: tree[Int]): Int =
{
    def multiplication_helper(children: List[tree[Int]]): Int =
    {
        if(children == Nil) 1
        else multiply (children.head) * multiplication_helper(children.tail)
    }
    original_tree match
    {
        case (Empty) => 1
        case (Node(value, children)) => value * multiply (children.head) * multiplication_helper(children.tail)
    }
}
val int_tree = create_full_tree(2, 2)
multiply(int_tree);

/////////////////////////////////////////////////////////////////////////////////
// 3a                                                                          //
/////////////////////////////////////////////////////////////////////////////////
def remove_repeats_depth[A](original_tree: tree[A]): tree[A] =
{
    def remove_repeats(children: List[tree[A]], visited_values: List[A], final_children: List[tree[A]]): List[tree[A]] =
    {
        children match
        {
            case (Nil) => final_children.reverse
            case (Empty :: tail) => remove_repeats(tail, visited_values, Empty::final_children)
            case (Node(value, children) :: tail) =>
                if(visited_values.contains(value))
                    remove_repeats(children ::: tail, visited_values, final_children)
                else
                    remove_repeats(tail, value :: visited_values, Node(value, remove_repeats(children, visited_values, List())) :: final_children)
        }
    }
    original_tree match
    {
        case (Empty) => Empty
        case (Node(value, children)) => Node(value, remove_repeats(children, List(value), List()))
    }
}
val int_tree = create_full_tree(2, 3)
remove_repeats_depth(int_tree);

// example of use
val int_tree =
    Node(1,List(
        Node(1,List(
            Node(9,
                List(Empty, Empty)),
            Node(8,
                List(Empty, Empty))
        )),
        Node(9,List(
            Node(6,
                List(Empty, Empty)),
            Node(2,
                List(Empty, Empty))
        ))
    ))
val corrected_tree = remove_repeats_depth(int_tree);
corrected_tree: tree[Int] =
    Node(1,List(
        Node(9,
            List(Empty, Empty)),
        Node(8,
            List(Empty, Empty)),
        Node(6,
            List(Empty, Empty)),
        Node(2,List(Empty, Empty))
    ))

/////////////////////////////////////////////////////////////////////////////////
// 3b                                                                          //
/////////////////////////////////////////////////////////////////////////////////
def remove_repeats_breadth[A](original_tree: tree[A]): tree[A] =
{
    def find_repeats(queue: List[tree[A]], visited_values: List[A], repeated_nodes: List[tree[A]]): List[tree[A]] =
    {
        queue match
        {
            case Nil => repeated_nodes
            case Empty :: tail => find_repeats(tail, visited_values, repeated_nodes)
            case Node(value, children) :: tail =>
                if(visited_values.contains(value))
                    find_repeats(tail ::: children, visited_values, Node(value, children) :: repeated_nodes)
                else
                    find_repeats(tail ::: children, value :: visited_values, repeated_nodes)
        }
    }
    def remove_repeats(children: List[tree[A]], repeated_nodes: List[tree[A]], final_children: List[tree[A]]): List[tree[A]] =
    {
        children match
        {
            case (Nil) => final_children//.reverse
            case (Empty :: tail) => remove_repeats(tail, repeated_nodes, Empty::final_children)
            case (Node(value, children) :: tail) =>
                if(repeated_nodes.contains(Node(value, children)))
                    remove_repeats(children ::: tail, repeated_nodes, final_children)
                else
                    remove_repeats(tail, repeated_nodes, Node(value, remove_repeats(children, repeated_nodes, List())) :: final_children)
        }
    }

    val repeats = find_repeats(List(original_tree), List(), List())
    original_tree match
    {
        case (Empty) => Empty
        case (Node(value, children)) => Node(value, remove_repeats(children, repeats, List()))
    }
}
val int_tree = create_full_tree(2, 3)
remove_repeats_breadth(int_tree);

// example of use
val int_tree =
    Node(1,List(
        Node(1,List(
            Node(9,
                List(Empty, Empty)),
            Node(8,
                List(Empty, Empty))
        )),
        Node(9,List(
            Node(6,
                List(Empty, Empty)),
            Node(2,
                List(Empty, Empty))
        ))
    ))
val corrected_tree = remove_repeats_breadth(int_tree);
corrected_tree: tree[Int] =
    Node(1,List(
        Node(9,List(
            Node(2,
                List(Empty, Empty)),
            Node(6,
                List(Empty, Empty))
        )),
        Node(8,
            List(Empty, Empty)), Empty, Empty))

    ))
