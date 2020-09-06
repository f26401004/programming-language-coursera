# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.
class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  All_My_Pieces = All_Pieces +
    [
     rotations([[0,0], [1,0], [-1,0], [0,-1], [-1,-1]]),
     rotations([[0,0], [1,0], [0,1]]),
     [[[0,-2], [0,-1], [0,0], [0,1], [0,2]],
      [[-2,0], [-1,0], [0,0], [1,0], [2,0]]]
    ]

  # your enhancements here
  def initialize(point_array, board)
    super
  end
  
  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end
end

class MyBoard < Board
  # your enhancements here
  attr_accessor :willCheat
  
  def initialize (game)
    self.willCheat = false
    
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
  end

  def next_piece
    if !self.willCheat
      @current_block = MyPiece.next_piece(self)
    else
      self.willCheat = false
      @score -= 100
      @current_block = MyPiece.new([[[0,0]]], self)
    end
    @current_pos = nil
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..locations.size-1).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end
end

class MyTetris < Tetris
  # your enhancements here
  def initialize
    super
    aditionalKeyBindings
  end

  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end
  
  def aditionalKeyBindings
    @root.bind('c', proc { if @board.score >= 100 && !@board.willCheat
                              @board.willCheat = true end})
    @root.bind('u', proc { @board.rotate_clockwise;
                           @board.rotate_clockwise })
  end
end