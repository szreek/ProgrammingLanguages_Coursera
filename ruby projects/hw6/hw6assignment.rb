# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece

  def initialize (point_array, board)
    super(point_array, board)
  end
  
  def self.next_piece (board)
    if board.cheat
      board.cheat = false
      board.can_cheat= true
      flipOnCheat (board)
    else
      MyPiece.new(All_Pieces.sample, board)
    end
  end
  
  def self.flipOnCheat (board)
    a = [[[0,0]]]
    MyPiece.new(a, board)
  end

 def self.rotations (point_array)
    rotate1 = point_array.map {|x,y| [-y,x]}  
    rotate2 = point_array.map {|x,y| [-x,-y]} 
    rotate3 = point_array.map {|x,y| [y,-x]}  
    [point_array, rotate1, rotate2, rotate3]  
  end
  
  All_Pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
                rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
                [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
                 [[0, 0], [0, -1], [0, 1], [0, 2]]],
                rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
                rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
                rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
                rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]), # Z
                rotations([[0, 0], [1, 0], [0, 1], [1, 1], [1, 2]]), #square with 1 more block
                [[[0, 0], [-1, 0], [1, 0], [2, 0], [3, 0]], # long with 1 more block (only needs two)
                 [[0, 0], [0, -1], [0, 1], [0, 2], [0, 3]]],
                rotations([[0, 0], [1, 0], [1, 1]])] # L made of 3 blocks
end

class MyBoard < Board

  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self)
    @cheat = false
    @can_cheat = true
    @score = 0
    @game = game
    @delay = 500
  end

  def rotate180
   rotate_clockwise
   rotate_clockwise
  end
  
  def  canCheat?
    if @score - 100 >= 0 && @can_cheat then true else false end
  end
    
  def cheatScore
    @score -=  100
    @can_cheat = false
  end

  def cheat
    @cheat
  end

  def cheat= (bool)
    @cheat = bool
  end

 def can_cheat
    @can_cheat
  end

  def can_cheat= (bool)
    @can_cheat = bool
  end

  def cheatMode
    if !game_over? and @game.is_running? and canCheat?
      cheatScore
      @cheat = true
    end
  end

  def next_piece
    @current_block = MyPiece.next_piece(self)
    @current_pos = nil
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..locations.size - 1).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end
    
end
class MyTetris < Tetris
  
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end
    
  def key_bindings  
    super
    @root.bind('u', proc {@board.rotate180})
    @root.bind('c', proc {@board.cheatMode})
  end
end
