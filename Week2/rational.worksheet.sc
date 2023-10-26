class Rational(x: Int, y: Int):
    require(y > 0, "denominator must be positive")

    private def gcd(a: Int, b: Int): Int = 
        if b == 0 then math.abs(a) else gcd(b, a % b)

    private val g = gcd(x, y)
    val numer = x / g
    val denom = y / g

    def this(x: Int) = this(x, 1)

    def add(r: Rational) = Rational(numer * r.denom + r.numer * denom, denom * r.denom)

    def neg = Rational(-numer, denom)

    def sub(r: Rational) = add(r.neg)

    override def toString(): String = s"$numer/$denom"

end Rational

extension (r: Rational)
    def abs: Rational = Rational(r.numer.abs, r.denom)

Rational(1,2).add(Rational(1,4)).toString()
Rational(1,2).sub(Rational(1,4)).toString()
Rational(1,2).neg
Rational(190)
Rational(-1354, 124).abs