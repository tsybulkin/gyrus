function Board(parent, cellSize, active) {

  var size = 14;
  active = typeof active !== 'undefined' ? active : false;

  // create parent dom element
  var board = document.createElement('div');
  board.className = 'board';
  parent.appendChild(board);
  this.div = board;

  var table = document.createElement('table');
  table.className = 'board_table';
  board.appendChild(table);

  // add rows
  _.times(size, function(n) {
    var row = document.createElement("tr");
    table.appendChild(row);

    // add cells
    _.times(size, function(n) {
      var cell = document.createElement("td");
      cell.style.width = cellSize + 'px';
      cell.style.height = cellSize + 'px';
      row.appendChild(cell);
    });

  });

  if (active) {
    var ch = document.createElement('div');
    ch.className = 'circle next';
    ch.style.width = cellSize-4 + 'px';
    ch.style.height = cellSize-4 + 'px';
    ch.style.position = 'absolute';
    // ch.style.visibility = "hidden"
    board.appendChild(ch);
    var next_checker = ch;

    // table.onmouseenter = function() { next_checker.style.visibility = "visible"; }
    // table.onmouseleave = function() { next_checker.style.visibility = "hidden"; }

    function _handleMouse(event) {
      // next_checker.style.visibility = "visible";

      // var x = Math.round(event.layerX / (cellSize+1)) + 1;
      // var y = 15 - Math.round(event.layerY / (cellSize+1));

      next_checker.style.left = Math.round(event.layerX / (cellSize+1)) * (cellSize+1) - cellSize/2 + 1 + 'px';
      next_checker.style.top = Math.round(event.layerY / (cellSize+1)) * (cellSize+1) - cellSize/2 - 1 + 'px';

    }

    table.onmousemove = _handleMouse;
  }

  this.addChecker = function(x, y, color) {
    var ch = document.createElement('div');
    ch.className = 'circle ' + color;
    ch.style.width = cellSize-4 + 'px';
    ch.style.height = cellSize-4 + 'px';
    ch.style.position = 'absolute';
    var cell_size = cellSize;
    var ch_size = cellSize;
    ch.style.left = cell_size*(x-1) - ch_size/2 + x + 'px';
    ch.style.bottom = cell_size*(y-1) - ch_size/2 + y + 'px';
    this.div.appendChild(ch);
  }
}

var b = new Board(document.getElementById('left_box'), 10);

b.addChecker(1,1,'black');
b.addChecker(2,2,'white');
b.addChecker(3,3,'black');
b.addChecker(3,2,'black');
b.addChecker(1,2,'black');

b.addChecker(5,5,'white');
b.addChecker(5,6,'black');
b.addChecker(6,6,'black');
b.addChecker(6,5,'black');
b.addChecker(4,5,'white');

b.addChecker(14,14,'white');
b.addChecker(14,15,'black');
b.addChecker(15,15,'black');
b.addChecker(15,14,'black');
b.addChecker(13,14,'white');

b = new Board(document.body, 40, true);
b.addChecker(1,1,'black');
b.addChecker(2,2,'white');
b.addChecker(3,3,'black');
b.addChecker(3,2,'black');
b.addChecker(1,2,'black');

b.addChecker(14,14,'white');
b.addChecker(14,15,'black');
b.addChecker(15,15,'black');
b.addChecker(15,14,'black');
b.addChecker(13,14,'white');