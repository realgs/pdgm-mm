// 1
def divide(input_list: List[Double]) : (List[Double], List[Double]) =
{
    def group (input_list: List[Double], xs: List[Double], ys: List[Double]) : (List[Double], List[Double])  =
        input_list match
        {
            case Nil => (xs, ys)
            case h::t =>
            {
                if(h > -1 && h < 1) group(t, xs:::List(h), ys)
                else group(t, xs, ys:::List(h))
            }
        }
    group(input_list, Nil, Nil)
}
divide (List(-3, -6, 8, -9, 13,1, 0, 1, 0.5, -0.5))

// 2
def list_length (xs: List[Double]) : Int  =
    xs match
    {
        case Nil => 0
        case h::t =>
        {
            if(h >= -1 && h <= 1) 1 + list_length(t)
            else list_length(t)
        }
    }
list_length(List(-12, -5, -1, -0.6, 0, 0.5, 1, 3, 20))

// 3
def concatenate_alternately (xs: List[Double], ys: List[Double]): List[Double] =
{
    def concatenate_alt(xs: List[Double], ys: List[Double]): List[Double] =
    {
        (xs, ys) match
        {
            case (Nil, Nil) => Nil
            case (Nil, _) => ys
            case (_, Nil) => xs
            case (xh::xt, yh::yt) => List(xh):::List(yh):::concatenate_alt(xt, yt)
        }
    }

    (xs, ys) match
    {
        case (Nil, Nil) => List(0.1) ::: List(0.1)
        case (Nil, _) => List(0.1) ::: ys
        case (_, Nil) => xs ::: List(0.1)
        case (_, _) => concatenate_alt(xs, ys)
        // case (xh::xt, yh::yt) => List(xh):::List(yh):::concatenate_alt(xt, yt)
    }
}
concatenate_alternately (List(5, 4, 3, 2), List(1, 2, 3, 4, 5, 6))
