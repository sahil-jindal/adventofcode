package day11

def preCompute(gridSerialNumber: Int): Array[Array[Int]] = {
    val gridOriginal = Array.ofDim[Int](300, 300)

    for(y <- 1 to 300; x <- 1 to 300) do {
        var rackId = x + 10;
        var powerLevel = rackId * y;
        powerLevel += gridSerialNumber;
        powerLevel *= rackId;
        powerLevel = (powerLevel % 1000) / 100;
        powerLevel -= 5;

        gridOriginal(y - 1)(x - 1) = powerLevel;
    }

    return gridOriginal
}

def solver(gridOriginal: Array[Array[Int]], D: Int): (Int, Int, Int) = {
    var maxTotalPower = Int.MinValue;
    var yMax = Int.MinValue;
    var xMax = Int.MinValue;
    var dMax = Int.MinValue;

    var grid = Array.ofDim[Int](300, 300)

    for (d <- 1 to D) {
        for (y <- 0 until 300 - d) {
            for (x <- 0 until 300) {
                grid(y)(x) += gridOriginal(y + d - 1)(x)
            }
        
            for (x <- 0 until 300 - d) {
                var totalPower = 0;

                for (i <- 0 until d) {
                    totalPower += grid(y)(x + i)
                }

                if (totalPower > maxTotalPower) {
                    maxTotalPower = totalPower;
                    yMax = y + 1;
                    xMax = x + 1;
                    dMax = d;
                }
            }
        }
    }
    
    return (xMax, yMax, dMax)
}

def evaluatorOne(grid: Array[Array[Int]]): String = {
    val (xMax, yMax, _) = solver(grid, 3)
    return s"$xMax,$yMax"
}

def evaluatorTwo(grid: Array[Array[Int]]): String = {
    val (xMax, yMax, dMax) = solver(grid, 300)
    return s"$xMax,$yMax,$dMax"
}

def hello(): Unit = {
    val inputLine = 5235
    val gridOriginal = preCompute(inputLine)
    println(s"Part One: ${evaluatorOne(gridOriginal)}")
    println(s"Part Two: ${evaluatorTwo(gridOriginal)}")
}