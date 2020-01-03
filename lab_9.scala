import java.util.concurrent._
import scala.util.DynamicVariable

val fork_join_pool = new ForkJoinPool

class task_scheduler
{
    def schedule[T](body: => T): ForkJoinTask[T] =
    {
        val task = new RecursiveTask[T] { def compute = body }

        if(Thread.currentThread.isInstanceOf[ForkJoinWorkerThread]) task.fork()
        else fork_join_pool.execute(task)

        task
    }
}

val scheduler = new DynamicVariable[task_scheduler](new task_scheduler)
def task[T](body: => T): ForkJoinTask[T] = { scheduler.value.schedule(body) }

def parallel[A, B](task_a: => A, task_b: => B): (A, B) =
{
    val right = task { task_b }
    val left = task_a
    (left, right.join())
}

// a funcion for time measures
def elapsed_time[R](block: => R): R =
{
    val start_time = System.nanoTime()
    val result = block                       // calling by name
    val end_time = System.nanoTime()
    println("Elapsed time: " + (end_time - start_time) + " ns")
    result
}


// 2.1
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
        Node[Int](random.nextInt(5) + 1, append_children(general_degree, Nil))
    }
}

def multiply(original_tree: tree[Int]): Int =
{
    def multiplication_helper(children: List[tree[Int]]): Int =
    {
        if(children == Nil) 1
        else multiply(children.head) * multiplication_helper(children.tail)
    }
    if(original_tree == Empty) 1
    else
    {
        val Node(value, children) = original_tree
        value * multiply(children.head) * multiplication_helper(children.tail)
    }
}

// generates max 2 threads
def parallel_multiply_1(original_tree: tree[Int]): Int =
{
    def multiplication_helper(children: List[tree[Int]]): Int =
    {
        if(children == Nil) 1
        else multiply(children.head) * multiplication_helper(children.tail)
    }
    if(original_tree == Empty) 1
    else
    {
        val Node(value, children) = original_tree
        val (left, right) = parallel(multiply(children.head), multiplication_helper(children.tail));
        value * (left * right)
    }
}

// generates max 3 threads
def parallel_multiply_2(original_tree: tree[Int]): Int =
{
    def multiplication_helper(children: List[tree[Int]]): Int =
    {
        if(children == Nil) 1
        else multiply(children.head) * multiplication_helper(children.tail)
    }
    if(original_tree == Empty) 1
    else
    {
        val Node(value, children) = original_tree
        val (left, right) = parallel(parallel_multiply_1(children.head), multiplication_helper(children.tail));
        value * (left * right)
    }
}

// generates (depth + 1) threads, max MAX_THREADS
def parallel_multiply_3(original_tree: tree[Int], threads_to_create: Int): Int =
{
    def multiplication_helper(children: List[tree[Int]]): Int =
    {
        if(children == Nil) 1
        else multiply(children.head) * multiplication_helper(children.tail)
    }
    if(original_tree == Empty) 1
    else
    {
        val Node(value, children) = original_tree

        if(threads_to_create > 0)
        {
            val (left, right) = parallel(parallel_multiply_3(children.head, threads_to_create - 1), multiplication_helper(children.tail));
            value * (left * right)
        }
        else
            value * multiply(children.head) * multiplication_helper(children.tail)
    }
}

// test
val int_tree = create_full_tree(3, 12)
val MAX_THREADS = 3;
val start_time = System.nanoTime;
multiply(int_tree);
val middle_time_1 = System.nanoTime;
parallel_multiply_1(int_tree);
val middle_time_2 = System.nanoTime;
parallel_multiply_2(int_tree);
val middle_time_3 = System.nanoTime;
parallel_multiply_3(int_tree, MAX_THREADS);
val end_time = System.nanoTime;

println("regular: " + (middle_time_1 - start_time));
println("parallel_1: " + (middle_time_2 - middle_time_1));
println("parallel_2: " + (middle_time_3 - middle_time_2));
println("parallel_3: " + (end_time - middle_time_3));


// 2.2
def duplicate [A] (elements: List[A], factors: List[Int]) :  List[A] =
{
    def replicate [A] (element: A, times: Int) :  List[A] =
        if(times > 0) List(element) ::: replicate(element, times - 1)
        else Nil

    if(elements == Nil || factors == Nil)
         Nil
    else
        replicate(elements.head, factors.head) ::: duplicate(elements.tail, factors.tail)
}

def parallel_duplicate [A] (elements: List[A], factors: List[Int]) :  List[A] =
{
    def parallel_replicate [A] (element: A, times: Int) :  List[A] =
        if(times > 0) List(element) ::: parallel_replicate(element, times - 1)
        else Nil

    if(elements == Nil || factors == Nil)
         Nil
    else
    {
        val (left, right) = parallel(parallel_replicate(elements.head, factors.head), parallel_duplicate(elements.tail, factors.tail))
        left ::: right
    }
}


def parallel_duplicate [A] (elements: List[A], factors: List[Int], parts_number: Int) :  List[A] =
{
    def duplicate_n_first [A](elements: List[A], factors: List[Int], number: Int) :  List[A] =
    {
        if(elements.length < number) duplicate(elements, factors)
        else
        {
            val (left_elements, right_elements) = elements.splitAt(number)
            val (left_factors, right_factors) = factors.splitAt(number)

            val (left, right) = parallel(duplicate(left_elements, left_factors), duplicate_n_first(right_elements, right_factors, number))
            left ::: right
        }
    }

    if(elements == Nil || factors == Nil)
         Nil
    else
    {
        val elements_number = elements.length;
        if(parts_number < 1 || parts_number > elements_number)
            duplicate(elements, factors)
        else
        {
            val elements_in_thread = elements_number / parts_number;
            duplicate_n_first(elements, factors, elements_in_thread)
        }
    }
}

// test
val PARTS = 3;
val start_time = System.nanoTime;
duplicate(List.range(1, 1000), List.fill(1000)(1000))
val middle_time = System.nanoTime;
parallel_duplicate(List.range(1, 1000), List.fill(1000)(1000), PARTS)
val end_time = System.nanoTime;

println("regular: " + (middle_time - start_time));
println("parallel: " + (end_time - middle_time));
