package day20

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}

case class Tile(id: Long, grid: List[Array[Char]]) {
    val size: Int = grid.length

    val top = grid(0).mkString
    val bottom = grid(size - 1).mkString
    val left = grid.map(_(0)).mkString
    val right = grid.map(_(size - 1)).mkString

    // Extract the borders of the tile in all 8 possible orientations
    val borders = Set(top, top.reverse, bottom, bottom.reverse, left, left.reverse, right, right.reverse)
  
    // Rotate the tile 90 degrees clockwise
    def rotate: Tile = {
        val newGrid = Array.ofDim[Char](size, size)
        
        for (i <- 0 until size; j <- 0 until size) {
            newGrid(j)(size - 1 - i) = grid(i)(j)
        }
        
        return Tile(id, newGrid.toList)
    }
  
    // Flip the tile horizontally
    def flipHorizontal: Tile = {
        val newGrid = Array.ofDim[Char](size, size)
        
        for (i <- 0 until size; j <- 0 until size) {
            newGrid(i)(size - 1 - j) = grid(i)(j)
        }
        
        return Tile(id, newGrid.toList)
    }
  
    // Generate all possible orientations of the tile
    def allOrientations: List[Tile] = {
        val rotations = Iterator.iterate(this, 4)(_.rotate).toList
        return rotations ++ rotations.map(_.flipHorizontal)
    }
  
    // Get the tile content without borders
    def withoutBorders: Array[Array[Char]] = {
        val innerSize = size - 2
        val innerGrid = Array.ofDim[Char](innerSize, innerSize)
        
        for (i <- 1 until size - 1; j <- 1 until size - 1) {
            innerGrid(i - 1)(j - 1) = grid(i)(j)
        }
        
        return innerGrid
    }
}

def groupLines(input: List[String]): List[List[String]] = {
    return input.foldLeft(List(List.empty[String])) {
        case (acc, "") => acc :+ List.empty[String]  
        case (acc, elem) => acc.init :+ (acc.last :+ elem)
    }.filter(_.nonEmpty)
}

def parseTiles(input: List[String]): List[Tile] = {
    return groupLines(input).map(lines => {
        val tileId = raw"Tile (\d+):".r.findFirstMatchIn(lines.head).get.group(1).toLong
        val grid = lines.tail.map(_.toCharArray)
        Tile(tileId, grid)
    })
}

def findCornerTiles(tiles: List[Tile]): List[Long] = {
    // Count each border's occurrences
    val borderCount = tiles.flatMap(_.borders).groupMapReduce(identity)(_ => 1)(_ + _)
    
    // Find tiles with exactly 4 unique borders (2 sides that match other tiles)
    return tiles.filter(_.borders.count(borderCount(_) == 1) == 4).map(_.id)
}

def findBorderMatches(tiles: List[Tile]): Map[Long, Set[String]] = {
    val borderMatches = MutableMap.empty[Long, Set[String]]
    
    // For each tile, store all borders that match with another tile
    for (tile <- tiles) {
        val matches = MutableSet.empty[String]

        for (otherTile <- tiles if tile.id != otherTile.id) {
            matches ++= tile.borders.intersect(otherTile.borders)
        }
        
        borderMatches(tile.id) = matches.toSet
    }
    
    return borderMatches.toMap
}

def orientCornerTile(tile: Tile, borderMatches: Map[Long, Set[String]]): Tile = {
    val matches = borderMatches(tile.id)
    
    // Find orientation where matches are on right and bottom
    return tile.allOrientations.find(orientedTile => {
        val topMatches = matches.contains(orientedTile.top)
        val rightMatches = matches.contains(orientedTile.right)
        val bottomMatches = matches.contains(orientedTile.bottom)
        val leftMatches = matches.contains(orientedTile.left)
        rightMatches && bottomMatches && !topMatches && !leftMatches
    }).getOrElse(tile)
}

def orientTileToMatchLeft(tile: Tile, leftBorder: String): Tile = {
    return tile.allOrientations.find(_.left == leftBorder).getOrElse(tile)
}

def orientTileToMatchTop(tile: Tile, topBorder: String): Tile = {
    return tile.allOrientations.find(_.top == topBorder).getOrElse(tile)
}

def isSeaMonsterAt(image: List[Array[Char]], pattern: List[String], x: Int, y: Int): Boolean = {
    return pattern.zipWithIndex.forall { case (line, py) =>
        line.zipWithIndex.forall { case (ch, px) =>
            ch != '#' || image(y + py)(x + px) == '#'
        }
    }
}
  
def countSeaMonsters(image: List[Array[Char]], pattern: List[String]): Int = {
    val patternHeight = pattern.length
    val patternWidth = pattern(0).length
    val imageHeight = image.length
    val imageWidth = image(0).length
    
    return (for {
        y <- 0 until imageHeight - patternHeight + 1
        x <- 0 until imageWidth - patternWidth + 1
        if isSeaMonsterAt(image, pattern, x, y)
    } yield 1).sum
}

def assembleImage(tiles: List[Tile]): Array[Array[Char]] = {
    val tileSize = tiles.head.size - 2  // We'll remove borders
    val tilesPerSide = math.sqrt(tiles.length).toInt
    val imageSize = tilesPerSide * tileSize
    
    // Create a map of tiles by ID for easier lookup
    val tilesById = tiles.map(t => t.id -> t).toMap
    
    // Find border matches between tiles
    val borderMatches = findBorderMatches(tiles)
    
    // Start with a corner tile
    val cornerTileId = findCornerTiles(tiles).head
    val cornerTile = tilesById(cornerTileId)

    // Orient the corner tile correctly (top-left corner)
    val orientedCorner = orientCornerTile(cornerTile, borderMatches)
    
    // Arrange tiles in a grid
    val arrangementGrid = Array.ofDim[Tile](tilesPerSide, tilesPerSide)

    def isIdNotPresent(id: Long) = arrangementGrid.flatten.forall(t => t == null || t.id != id)

    def findMatchingTileId(border: String): Long = {
        return borderMatches.view.filterKeys(isIdNotPresent).collectFirst { 
            case (id, matches) if matches.contains(border) => id
        }.get
    }

    arrangementGrid(0)(0) = orientedCorner
    
    // Fill the first row
    for (x <- 1 until tilesPerSide) {
        val leftBorder = arrangementGrid(0)(x - 1).right  // Right border of left tile
        val matchingTileId = findMatchingTileId(leftBorder)
        arrangementGrid(0)(x) = orientTileToMatchLeft(tilesById(matchingTileId), leftBorder)
    }

    for (y <- 1 until tilesPerSide) {
        val topBorder = arrangementGrid(y - 1)(0).bottom  // Bottom border of top tile
        val matchingTileId = findMatchingTileId(topBorder) 
        arrangementGrid(y)(0) = orientTileToMatchTop(tilesById(matchingTileId), topBorder)
    }
    
    // Fill remaining rows
    for (y <- 1 until tilesPerSide; x <- 1 until tilesPerSide) {
        val topBorder = arrangementGrid(y - 1)(x).bottom  // Bottom border of top tile
        val matchingTileId = findMatchingTileId(topBorder) 
        val orientedTile = orientTileToMatchTop(tilesById(matchingTileId), topBorder)
    
        // ensure it also matches the left tile
        val leftBorder = arrangementGrid(y)(x - 1).right  // Right border of left tile
    
        arrangementGrid(y)(x) = orientedTile.allOrientations
            .find(t => t.left == leftBorder && t.top == topBorder.reverse)
            .getOrElse(orientedTile)
    }
    
    // Create the final image by removing borders
    val image = Array.ofDim[Char](imageSize, imageSize)
    
    for (tileY <- 0 until tilesPerSide; tileX <- 0 until tilesPerSide) {
        val tile = arrangementGrid(tileY)(tileX)
        val innerGrid = tile.withoutBorders
      
        for (y <- 0 until tileSize; x <- 0 until tileSize) {
            image(tileY * tileSize + y)(tileX * tileSize + x) = innerGrid(y)(x)
        }
    }
    
    return image
}

def countRoughWaters(tiles: List[Tile]): Int = {
    // Build the full image by arranging tiles
    val image = assembleImage(tiles).toList
    
    // Define the sea monster pattern
    val seaMonsterPattern = List(
      "                  # ",
      "#    ##    ##    ###",
      " #  #  #  #  #  #   "
    )
    
    // Convert the image to Tile to use rotation and flip methods
    val imageTile = Tile(0, image)
    
    // Try all orientations and count sea monsters
    val (orientedImage, monsterCount) = imageTile.allOrientations
        .map(tile => (tile, countSeaMonsters(tile.grid, seaMonsterPattern)))
        .maxBy(_._2)
    
    // Count total '#' in the image and subtract sea monster '#'s
    val totalHashes = orientedImage.grid.flatten.count(_ == '#')
    val hashesInMonster = seaMonsterPattern.flatten.count(_ == '#')
    
    return totalHashes - (hashesInMonster * monsterCount)
}

def evaluatorOne(tiles: List[Tile]): Long = findCornerTiles(tiles).map(_.toLong).product
def evaluatorTwo(tiles: List[Tile]): Int = countRoughWaters(tiles)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day20.txt") match {
        case Success(lines) => {
            val input = parseTiles(lines)
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}