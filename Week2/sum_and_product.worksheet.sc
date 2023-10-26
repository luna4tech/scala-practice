def id(x: Int): Int = x
def cube(x: Int): Int = x * x * x
def fact(x: Int): Int = 
    if x <= 0 then 1 else x * fact(x - 1)

// ------------------------
// 0. basic functions
// ------------------------
def sumInts0(a: Int, b: Int): Int = 
    if a > b then 0 else a + sumInts0(a+1, b)

def sumCubes0(a: Int, b: Int): Int = 
    if a > b then 0 else cube(a) + sumCubes0(a+1, b)

def sumFactorials0(a: Int, b: Int): Int = 
    if a > b then 0 else fact(a) + sumFactorials0(a + 1, b)

sumInts0(2, 5)
sumCubes0(2, 5)

// ------------------------
// 1. Higher order functions - pass inner function as argument
// ------------------------
def sum1(f: Int => Int, a: Int, b: Int): Int = 
    if a > b then 0 else f(a) + sum1(f, a+1, b)

def sumTailRec(f: Int => Int, a: Int, b: Int): Int = 
    def loop(a: Int, acc: Int): Int =
        if a > b then acc
        else loop(a + 1, acc + f(a))
    loop(a, 0)

def sumInts1(a: Int, b: Int) = sum1(id, a, b)
def sumCubes1(a: Int, b: Int) = sum1(cube, a, b)
def sumFactorials1(a: Int, b: Int) = sum1(fact, a, b)

sumInts1(2, 5)
sumCubes1(2, 5)

// -------------------------
// 2. Anonymous functions
// -------------------------
def sumInts2(a: Int, b: Int) = sum1(x => x, a, b)
def sumCubes2(a: Int, b: Int) = sum1(x => x * x * x, a, b)

sumInts2(2, 5)
sumCubes2(2, 5)

// --------------------------
// 3. Higher order functions - return function
// --------------------------
def sum3(f: Int => Int): (Int, Int) => Int = 
    def sumF(a: Int, b: Int): Int = 
        if a > b then 0
        else f(a) + sumF(a + 1, b)
    sumF

def sumInts3 = sum3(x => x)     // type: (int, int) => int
def sumCubes3 = sum3(x => x * x * x)// type: (int, int) => int

sumInts3(2, 5)
sumCubes3(2, 5)

// left association: f(g)(a) -> (f(g))(a)
sum3(x => x)(2,5)
sum3(x => x * x * x)(2,5)

// --------------------------
// 4. Currying: Convert function with 3 args to 1 arg and 2 args
//              by returning a function which takes 2 args
//              Special syntax in Scala to get rid of temp inner function name
// --------------------------
def sum4(f: Int => Int)(a: Int, b: Int): Int = 
    if a > b then 0 else f(a) + sum4(f)(a + 1, b)

sum4(x => x)(2,5)
sum4(x => x * x * x)(2,5)

def product4(f: Int => Int)(a: Int, b: Int): Int = 
    if a > b then 1 else f(a) * product4(f)(a + 1,b)

product4(x => x * x)(1,5)

def factorial(a: Int) = product4(x => x)(1, a)
factorial(5)

// ---------------------------
// 5. Exercise: common function for sum and product
// ---------------------------
def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = 
    def recur(a: Int): Int = 
        if(a > b) then zero
        else combine(f(a), recur(a + 1))
    recur(a)

def sum5(f: Int => Int) = mapReduce(f, (x,y) => x + y, 0)
sum5(x => x*x*x)(2,5)

def product5(f: Int => Int) = mapReduce(f, (x,y) => x*y, 1)
product5(x => x*x)(1,5)