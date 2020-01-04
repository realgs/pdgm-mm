import java.util.concurrent.{ForkJoinPool, ForkJoinTask, ForkJoinWorkerThread, RecursiveTask}

import scala.util.DynamicVariable


object ConcurrentTask {
 private val forkJoinPool = new ForkJoinPool
  private abstract class TaskScheduler {
    def schedule[T](body: => T): ForkJoinTask[T]
  }
  private class DefaultTaskScheduler extends TaskScheduler {
    def schedule[T](body: => T): ForkJoinTask[T] = {
      val t = new RecursiveTask[T] {
        def compute = body
      }
      Thread.currentThread match {
        case wt: ForkJoinWorkerThread =>
          t.fork()
        case _ =>
          forkJoinPool.execute(t)
      }
      t
    }
  }

  private val scheduler =
    new DynamicVariable[TaskScheduler](new DefaultTaskScheduler)

  private def task[T](body: => T): ForkJoinTask[T] = {
    scheduler.value.schedule(body)
  }

  def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
    val right = task {
      taskB
    }
    val left = taskA
    (left, right.join())
  }
}
