newline := Character cr asString, Character nl asString.

Number extend [
  modIndex: index [
    "modulo, but suitable for 1-based array indexing"
    | mod |
    mod := self \\ index.
    (mod == 0) ifTrue: [^index] ifFalse: [^mod].
  ]

  flipped: count [
    "'invert' the lowest count bits in this number"
    | result lthbit htlbit |
    result := 0.
    1 to: count do: [:i|
      lthbit := 1 bitShift: (i - 1).
      htlbit := 1 bitShift: (count - i).
      ((self bitAnd: htlbit) == 0) ifFalse: [
        result := result bitOr: lthbit.
      ]
    ].
    ^result.
  ]
]

Array extend [
  rotated [
    "Rotates a square 2D array one degree clockwise"
    | size result row element |
    size := self size.
    result := Array new: size.
    1 to: size do: [ :newY |
      row := Array new: size.
      result at: newY put: row.
      1 to: size do: [ :newX |
        element := (self at: (size - newX + 1)) at: newY.
        row at: newX put: element.
      ].
    ].
    ^result.
  ]
  flipped [
    "Flips a square 2D array (about the Y axis)"
    | size result row element |
    size := self size.
    result := Array new: size.
    1 to: size do: [ :y |
      row := Array new: size.
      result at: y put: row.
      1 to: size do: [ :x |
        element := (self at: y) at: (size - x + 1).
        row at: x put: element.
      ].
    ].
    ^result.
  ]
]

Object subclass: Tile [
  | id size pixels orientation neighbors edges |

  idRegex := 'Tile (\d+):' asRegex.

  Tile class >> new: lines [
    | r |
    r := super new.
    r init: lines.
    ^r
  ]
  init: lines [
    | allPixels |
    id := (((lines first) =~ idRegex) at: 1) asNumber.
    size := (lines size) - 1.
    allPixels := Array new: size.
    1 to: size do: [ :i | allPixels at: i put: (lines at: (i + 1)) ]. 
    pixels := allPixels.
    neighbors := Array new: 4.
    self initEdges.
  ]
  initEdges [
    | htlbit lthbit pixel |
    edges := Array with: 0 with: 0 with: 0 with: 0.
    1 to: size do: [ :i |
      htlbit := 1 bitShift: (size - i).
      lthbit := 1 bitShift: (i - 1). 

      "top side"
      pixel := (pixels at: 1) at: i.
      (pixel = $#) ifTrue: [
        edges at: 1 put: ((edges at: 1) bitOr: htlbit).
      ].

      "right side"
      pixel := (pixels at: i) at: size.
      (pixel = $#) ifTrue: [
        edges at: 2 put: ((edges at: 2) bitOr: htlbit).
      ].

      "bottom side"
      pixel := (pixels at: size) at: i.
      (pixel = $#) ifTrue: [
        edges at: 3 put: ((edges at: 3) bitOr: lthbit).
      ].

      "left side"
      pixel := (pixels at: i) at: 1.
      (pixel = $#) ifTrue: [
        edges at: 4 put: ((edges at: 4) bitOr: lthbit).
      ].
    ].
  ]
  id [ ^id ]
  size [ ^size ]
  isCorner [
    |actualNeighbors|
    actualNeighbors := neighbors select: [ :n | n notNil ].
    ^((actualNeighbors size) == 2).
  ]
  matching: edge [
    | localEdge |
    1 to: 4 do: [ :i |
      localEdge := edges at: i.
      (localEdge == edge) ifTrue: [
        ^(i + 4).
      ].
      localEdge := localEdge flipped: size.
      (localEdge == edge) ifTrue: [
        ^i.
      ].
    ].
    ^nil.
  ]
  orientAsTopLeft [
    (self isCorner) ifFalse: [ ^self error: 'not a corner' ].
    1 to: 4 do: [ :i |
      (neighbors at: i) ifNotNil: [
        (neighbors at: ((i + 1) modIndex: 4)) ifNotNil: [
          orientation := (7 - i) modIndex: 4.
          ^self.
        ]
      ]
    ].
  ]
  orient: edge towards: direction [
    | localEdge |
    orientation ifNotNil: [
      ^self error: 'Already oriented'.
    ].
    1 to: 4 do: [ :i |
      localEdge := edges at: i.
      (localEdge == edge) ifTrue: [
        orientation := ((i + direction - 1) modIndex: 4) + 4.
        ^self.
      ].
      localEdge := localEdge flipped: size.
      (localEdge == edge) ifTrue: [
        orientation := ((direction - i + 1) modIndex: 4).
        ^self.
      ].
    ].
    ^self error: 'cannot find it'.
  ]
  neighborIn: direction [
    | index |
    index := self indexOf: direction.
    ^neighbors at: index.
  ]
  edgeInDirection: direction [
    | edge |
    edge := edges at: (self indexOf: direction).
    (orientation > 4)
      ifTrue: [ ^edge flipped: size ]
      ifFalse: [ ^edge ].
  ]
  indexOf: direction [
    "1 is north, 2 is east, 3 is south, 4 is west"
    orientation ifNil: [
      ^self error: 'Must be oriented first'.
    ].
    (orientation > 4)
      ifTrue: [^(orientation - direction + 1) modIndex: 4]
      ifFalse: [^(direction - orientation + 1) modIndex: 4].
    ^(direction - orientation + 1) modIndex: 4.
  ]
  fits: edge [
    | matching |
    matching := edges select: [ :e |
      e == edge or: [(e flipped: size) == edge].
    ].
    ^((matching size) > 0).
  ]
  findFit: otherTile [
    | edge |
    1 to: 4 do: [ :i |
      edge := edges at: i.
      (otherTile fits: edge) ifTrue: [
        (neighbors at: i put: (otherTile id)).
        ^true.
      ].
      edge := edge flipped: size.
      (otherTile fits: edge) ifTrue: [
        (neighbors at: i put: (otherTile id)).
        ^true.
      ].
    ].
    ^false.
  ]
  toPixels [
    ^self toPixelsIn: orientation.
  ]
  toPixelsIn: orientation [
    (orientation > 4) ifTrue: [^(self toPixelsIn: ((6 - orientation) modIndex: 4)) flipped].
    (orientation > 1) ifTrue: [^(self toPixelsIn: orientation - 1) rotated].
    ^pixels.
  ]
  printOn: stream [
    super printOn: stream.
    stream nextPutAll: ' with id '.
    id printOn: stream.
    stream nextPutAll: ' and edges '.
    edges printOn: stream.
    stream nextPutAll: ' and neighbors '.
    neighbors printOn: stream.
    stream nextPutAll: ' and orientation '.
    orientation printOn: stream.
  ]
].

Object subclass: Grid [
  | tiles size |

  Grid class >> new: allTiles [
    | r |
    r := super new.
    r init: allTiles.
    ^r
  ]

  init: allTiles [
    | sidelength tileLookup prev newrow id tile edge |
    sidelength := ((allTiles size) sqrt) truncated.
    tiles := Array new: (allTiles size).

    "Find how the pieces fit together"
    allTiles do: [ :t1 |
      allTiles do: [ :t2 |
        (t1 = t2) ifFalse: [
          t1 findFit: t2.
        ]
      ].
    ].

    tileLookup := LookupTable new.
    allTiles do: [ :each | tileLookup at: (each id) put: each ].

    "Put together the puzzle"
    tile := allTiles detect: [:tile | tile isCorner ].
    tile orientAsTopLeft.
    tiles at: 1 put: tile.
    2 to: (allTiles size) do: [ :i |
      newrow := (i \\ sidelength) == 1.
      prev := newrow
        ifTrue: [tiles at: (i - sidelength)]
        ifFalse: [tiles at: (i - 1)].
      id := prev neighborIn: (newrow ifTrue: [3] ifFalse: [2]).
      tile := tileLookup at: id ifAbsent: [].

      edge := prev edgeInDirection: (newrow ifTrue: [3] ifFalse: [2]).
      tile orient: edge towards: (newrow ifTrue: [1] ifFalse: [4]).
      tiles at: i put: tile.
    ].
  ]

  corners [
    ^tiles select: [ :t | t isCorner ].
  ]

  toPixels [
    "Returns a 2D array of pixels"
    | gridlength tilelength size result tilePixels rowIndex row colIndex |
    gridlength := ((tiles size) sqrt) truncated.
    tilelength := ((tiles first) size) - 2.
    size := gridlength * tilelength.
    result := Array new: size.
    tiles keysAndValuesDo: [ :index :tile |
      tilePixels := tile toPixels.
      1 to: tilelength do: [ :y |
        rowIndex := (tilelength * ((index - 1) / gridlength) truncated) + y.
        row := result at: rowIndex.
        row ifNil: [
          row := Array new: size.
          result at: rowIndex put: row.
        ].
        1 to: tilelength do: [ :x |
          colIndex := (tilelength * ((index - 1) \\ gridlength)) + x.
          row at: colIndex put: ((tilePixels at: (y + 1)) at: (x + 1)).
        ].
      ].
    ].
    ^result.
  ]
]

Object subclass: MonsterHunter [
  | grid |

  MonsterWidth := 20.
  MonsterHeight := 3.
  MonsterRow1 := '                  # '.
  MonsterRow2 := '#    ##    ##    ###'.
  MonsterRow3 := ' #  #  #  #  #  #   '.

  MonsterHunter class >> new: aGrid [
    | r |
    r := super new.
    r init: aGrid.
    ^r
  ]

  init: aGrid [
    grid := aGrid.
  ]

  determineWaterRoughness [
    | pixels sum |
    pixels := self findAllMonsters.
    pixels ifNil: [^self error: 'No monsters found' ].
    "we have already marked every sea monster on the map"
    "so just count the hashtags"
    sum := 0.
    pixels do: [ :row |
      (row asString) printNl. 
      row do: [ :char |
        (char = $#) ifTrue: [ sum := sum + 1 ].
      ].
    ].
    ^sum.
  ]

  findAllMonsters [
    | pixels sum |
    pixels := grid toPixels.
    1 to: 4 do: [ :i |
      sum := self findMonsters: pixels.
      (sum > 0) ifTrue: [^pixels].
      pixels := pixels rotated.
    ].
    pixels := pixels flipped.
    1 to: 4 do: [ :i |
      sum := self findMonsters: pixels.
      (sum > 0) ifTrue: [^pixels].
      pixels := pixels rotated.
    ].
    ^nil.
  ]

  findMonsters: pixels [
    | count |
    count := 0.
    1 to: ((pixels size) + 1 - MonsterHeight) do: [ :row |
      1 to: ((pixels size) + 1- MonsterWidth) do: [ :column |
        (self isMonster: pixels atRow: row andColumn: column) ifTrue: [count := count + 1].
      ].
    ].
    ^count.
  ]

  isMonster: pixels atRow: row andColumn: column [
    | char |
    self fromRow: row andColumn: column monsterDo: [ :r :c |
      char := (pixels at: r) at: c.
      (char = $#) ifFalse: [^false].
    ].
    "we have found a monster! save some backtracking by Oing it out now."
    self fromRow: row andColumn: column monsterDo: [ :r :c |
      (pixels at: r) at: c put: $O.
    ].
    ^true.
  ]

  fromRow: row andColumn: column monsterDo: block [
    MonsterRow1 keysAndValuesDo: [ :i :char |
      (char = $#) ifTrue: [ block value: row value: (column + i - 1). ].
    ].
    MonsterRow2 keysAndValuesDo: [ :i :char |
      (char = $#) ifTrue: [ block value: (row + 1) value: (column + i - 1) ].
    ].
    MonsterRow3 keysAndValuesDo: [ :i :char |
      (char = $#) ifTrue: [ block value: (row + 2) value: (column + i - 1) ].
    ].
  ]
]

fstream := FileStream open: 'input' mode: FileStream read.
input := fstream contents.
fstream close.

tileStrings := (input tokenize: newline, newline) collect: [ :each | each tokenize: newline ].
tiles := tileStrings collect: [ :each | Tile new: each ].
grid := Grid new: tiles.

answer := 1.
(grid corners) do: [ :corner |
  answer := answer * (corner id).
].
answer printNl.
hunter := MonsterHunter new: grid.
(hunter determineWaterRoughness) printNl.