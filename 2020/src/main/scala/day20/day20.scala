package day20

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable
import scala.util.boundary, boundary.break

case class Tile(id: Long, grid: List[Array[Char]]) {
    val size: Int = grid.length
  
    // Extract the borders of the tile in all 8 possible orientations
    lazy val borders: Set[String] = {
        val top = grid(0).mkString
        val bottom = grid(size - 1).mkString
        val left = grid.map(_(0)).mkString
        val right = grid.map(_(size - 1)).mkString
    
        Set(top, top.reverse, bottom, bottom.reverse, left, left.reverse, right, right.reverse)
    }
  
    // Get the current borders in clockwise order: top, right, bottom, left
    def currentBorders: Array[String] = {
        val top = grid(0).mkString
        val bottom = grid(size - 1).mkString
        val left = grid.map(_(0)).mkString
        val right = grid.map(_(size - 1)).mkString
    
        return Array(top, right, bottom, left)
    }
  
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
        val flipped = flipHorizontal
        return rotations ++ Iterator.iterate(flipped, 4)(_.rotate).toList
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
    return groupLines(input).map { lines =>
        val tileId = lines(0).drop(5).dropRight(1).toLong
        val grid = lines.drop(1).map(_.toCharArray)
        Tile(tileId, grid)
    }
}

def findCornerTiles(tiles: List[Tile]): List[Long] = {
    val borderCount = mutable.Map.empty[String, Int].withDefaultValue(0)
    
    // Count each border's occurrences
    for (tile <- tiles; border <- tile.borders) {
        borderCount(border) += 1
    }
    
    // Find tiles with exactly 4 unique borders (2 sides that match other tiles)
    return tiles.filter(_.borders.count(borderCount(_) == 1) == 4).map(_.id)
}

def findBorderMatches(tiles: List[Tile]): Map[Long, Set[String]] = {
    val borderMatches = mutable.Map.empty[Long, Set[String]]
    
    // For each tile, store all borders that match with another tile
    for (tile <- tiles) {
        val matches = mutable.Set.empty[String]
        for (otherTile <- tiles if tile.id != otherTile.id) {
            val sharedBorders = tile.borders.intersect(otherTile.borders)
            matches ++= sharedBorders
        }
        
        borderMatches(tile.id) = matches.toSet
    }
    
    return borderMatches.toMap
}

def orientCornerTile(tile: Tile, borderMatches: Map[Long, Set[String]]): Tile = {
    val matches = borderMatches(tile.id)
    
    // Find orientation where matches are on right and bottom
    return tile.allOrientations.find(orientedTile => {
        val borders = orientedTile.currentBorders
        val topMatches = matches.contains(borders(0))
        val rightMatches = matches.contains(borders(1))
        val bottomMatches = matches.contains(borders(2))
        val leftMatches = matches.contains(borders(3))
        rightMatches && bottomMatches && !topMatches && !leftMatches
    }).getOrElse(tile)
}

def orientTileToMatchLeft(tile: Tile, leftBorder: String): Tile = {
    return tile.allOrientations.find(_.currentBorders(3) == leftBorder).getOrElse(tile)
}

def orientTileToMatchTop(tile: Tile, topBorder: String): Tile = {
    return tile.allOrientations.find(_.currentBorders(0) == topBorder).getOrElse(tile)
}

def isSeaMonsterAt(image: List[Array[Char]], pattern: List[String], x: Int, y: Int): Boolean = {
    val patternHeight = pattern.length
    val patternWidth = pattern(0).length
    
    boundary {
        for (py <- 0 until patternHeight; px <- 0 until patternWidth) {
            if (pattern(py)(px) == '#' && image(y + py)(x + px) != '#') {
                break(false)
            }
        }
    
        true
    }
}
  
def countSeaMonsters(image: List[Array[Char]], pattern: List[String]): Int = {
    val patternHeight = pattern.length
    val patternWidth = pattern(0).length
    val imageHeight = image.length
    val imageWidth = image(0).length
    var count = 0
    
    for (y <- 0 until imageHeight - patternHeight + 1) {
        for (x <- 0 until imageWidth - patternWidth + 1) {
            if (isSeaMonsterAt(image, pattern, x, y)) {
                count += 1
            }
        }
    }
    
    return count
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
    
    // Arrange tiles in a grid
    val arrangementGrid = Array.ofDim[Tile](tilesPerSide, tilesPerSide)
    
    // Orient the corner tile correctly (top-left corner)
    val orientedCorner = orientCornerTile(cornerTile, borderMatches)
    arrangementGrid(0)(0) = orientedCorner
    
    // Fill the first row
    for (x <- 1 until tilesPerSide) {
        val leftTile = arrangementGrid(0)(x - 1)
        val leftBorder = leftTile.currentBorders(1)  // Right border of left tile
      
        val matchingTileId = borderMatches
            .filter(m => m._2.contains(leftBorder) && !arrangementGrid.flatten.exists(t => t != null && t.id == m._1))
            .keys.head
      
        val matchingTile = tilesById(matchingTileId)
        val orientedTile = orientTileToMatchLeft(matchingTile, leftBorder)
        arrangementGrid(0)(x) = orientedTile
    }
    
    // Fill remaining rows
    for (y <- 1 until tilesPerSide) {
        for (x <- 0 until tilesPerSide) {
            val topTile = arrangementGrid(y - 1)(x)
            val topBorder = topTile.currentBorders(2)  // Bottom border of top tile
        
            val matchingTileId = borderMatches
                .filter(m => m._2.contains(topBorder) && !arrangementGrid.flatten.exists(t => t != null && t.id == m._1))
                .keys.head
        
            val matchingTile = tilesById(matchingTileId)
            val orientedTile = orientTileToMatchTop(matchingTile, topBorder)
        
        // If not in the first column, ensure it also matches the left tile
            if (x > 0) {
                val leftTile = arrangementGrid(y)(x - 1)
                val leftBorder = leftTile.currentBorders(1)  // Right border of left tile
          
                val finalOrientedTile = orientedTile.allOrientations
                    .find(t => t.currentBorders(3) == leftBorder && t.currentBorders(0) == topBorder.reverse)
                    .getOrElse(orientedTile)
          
                arrangementGrid(y)(x) = finalOrientedTile
            } else {
                arrangementGrid(y)(x) = orientedTile
            }
        }
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