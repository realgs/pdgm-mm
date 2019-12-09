// class for tests
class Point(x_value: Int, y_value: Int) extends Debug
{
    var x: Int = x_value
    var y: Int = y_value
    var a: String = "test"
}

// 1
trait Debug
{
    def print_class_name() =
    {
        // println("Klasa: " + getClass.toString.substring(getClass.toString.lastIndexOf('$') + 1))
        println("Klasa: " + getClass.getSimpleName)
    }
}

var p : Point = new Point(3, 4);
p.print_class_name();


// 2
trait Debug
{
    def print_fields() = getClass.getDeclaredFields.foreach(field =>
        {
            field.setAccessible(true)
            println("Pole: " + field.getName + " => " + field.getType + ", "  + field.get(this))
        })
}

var p : Point = new Point(3, 4);
p.print_fields();


// 3
trait Debug
{
    def get_class_name(): String =
        "Klasa: " + getClass.getSimpleName

    def get_fields_array() = getClass.getDeclaredFields.map(field =>
        {
            field.setAccessible(true)
            (field.getName, field.getType, field.get(this))
        })
}

var p : Point = new Point(3, 4);
p.get_class_name();
p.get_fields_array();
