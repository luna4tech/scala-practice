class Rational(x: Int, y: Int):
    require(y > 0, "denominator must be positive")

    private def gcd(a: Int, b: Int): Int = 
        if b == 0 then math.abs(a) else gcd(b, a % b)

    private val g = gcd(x, y)
    val numer = x / g
    val denom = y / g

    def this(x: Int) = this(x, 1)

    def +(r: Rational) = Rational(numer * r.denom + r.numer * denom, denom * r.denom)
 
    def unary_- = Rational(-numer, denom)

    def -(r: Rational) = this + (-r)

    override def toString(): String = s"$numer/$denom"

end Rational

extension (r: Rational)
    def abs: Rational = Rational(r.numer.abs, r.denom)

val x = Rational(1,2)
val y = Rational(1,4)
(x + y).toString
(x - y).toString
-x
Rational(-1354, 124).abs