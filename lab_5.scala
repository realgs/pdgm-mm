/////////////////////////////////////////////////////////////////////////////////
// 1                                                                           //
/////////////////////////////////////////////////////////////////////////////////
sealed trait K_tree[+A]
case object Empty extends K_tree[Nothing]
case class Node[+A](value: A, children: List[K_tree[A]]) extends K_tree[A]

def create_full_K_tree (K: Int, N: Int): K_tree[Int] =
{
    if(N == 0) Empty
    else
    {
        def append_children(k: Int, children: List[K_tree[Int]]): List[K_tree[Int]] =
        {
            if(k == 0) children
            else append_children(k - 1, create_full_K_tree(K, N - 1)::children)
        }
        val random = scala.util.Random
        Node[Int](random.nextInt(10), append_children(K, Nil))
    }
}
val k_int_tree = create_full_K_tree(2, 3)

/////////////////////////////////////////////////////////////////////////////////
// 2                                                                           //
/////////////////////////////////////////////////////////////////////////////////
def multiply(k_tree: K_tree[Int]): Int =
{
    def multiplication_helper(childrem: List[K_tree[Int]]): Int =
    {
        if(childrem == Nil) 1
        else multiply (childrem.head) * multiplication_helper(childrem.tail)
    }
    k_tree match
    {
        case (Empty) => 1
        case (Node(value, children)) => value * multiply (children.head) * multiplication_helper(children.tail)
    }
}
val k_int_tree = create_full_K_tree(2, 2)
multiply(k_int_tree);

/////////////////////////////////////////////////////////////////////////////////
// 3a                                                                          //
/////////////////////////////////////////////////////////////////////////////////
def remove_repeats_depth[A](k_tree: K_tree[A]): K_tree[A] =
{
    def remove_repeats(children: List[K_tree[A]], visited_values: List[A], final_children: List[K_tree[A]]): List[K_tree[A]] =
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
    k_tree match
    {
        case (Empty) => Empty
        case (Node(value, children)) => Node(value, remove_repeats(children, List(value), List()))
    }
}
val k_int_tree = create_full_K_tree(2, 3)
remove_repeats_depth(k_int_tree);

// example of use
val k_int_tree =
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
val corrected_tree = remove_repeats_depth(k_int_tree);
corrected_tree: K_tree[Int] =
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
def remove_repeats_breadth[A](k_tree: K_tree[A]): K_tree[A] =
{
    def find_repeats(queue: List[K_tree[A]], visited_values: List[A], repeated_nodes: List[K_tree[A]]): List[K_tree[A]] =
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
    def remove_repeats(children: List[K_tree[A]], repeated_nodes: List[K_tree[A]], final_children: List[K_tree[A]]): List[K_tree[A]] =
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

    val repeats = find_repeats(List(k_tree), List(), List())
    k_tree match
    {
        case (Empty) => Empty
        case (Node(value, children)) => Node(value, remove_repeats(children, repeats, List()))
    }
}
val k_int_tree = create_full_K_tree(2, 3)
remove_repeats_breadth(k_int_tree);

// example of use
val k_int_tree =
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
val corrected_tree = remove_repeats_breadth(k_int_tree);
corrected_tree: K_tree[Int] =
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
