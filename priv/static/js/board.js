function Board(parent, cellSize, callback) {

    var self = this;
  var size = 14;
  active = typeof active !== 'undefined' ? active : false;
  // Mouse coordinates
  var x = 0, y = 0;

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

  if (callback) {
    var ch = document.createElement('div');
    ch.className = 'circle next';
    ch.style.width = cellSize-4 + 'px';
    ch.style.height = cellSize-4 + 'px';
    ch.style.position = 'absolute';
    board.appendChild(ch);
    var next_checker = ch;

    function _handleMouseMove(event) {
      x = Math.round(event.layerX / (cellSize+1)) + 1;
      y = 15 - Math.round(event.layerY / (cellSize+1));

      next_checker.style.left = Math.round(event.layerX / (cellSize+1)) * (cellSize+1) - cellSize/2 + 1 + 'px';
      next_checker.style.top = Math.round(event.layerY / (cellSize+1)) * (cellSize+1) - cellSize/2 + 'px';

    }

    function _handleClick() {
      // TODO: check if empty
      callback(x,y,'white');
    }

    table.onmousemove = _handleMouseMove;
    board.onclick = _handleClick;
  }


}

function BoardConnection(board, baseUrl, color, level) {
  var state = 'init';
  var self = this;

  this.send = function(obj) {
    var json = JSON.stringify(obj);
    self.ws.send(json);
  }

  function handle_message(msg) {
    console.log(msg);
    var action = msg[0];
    if ('bot_move' == action) {
        var x = msg[1];
        var y = msg[2];
        board.addChecker(x,y,'black');
    }
    else if('game_over' == action) {
        if ('man_lost' == msg[1]) {
            var x = msg[2];
            var y = msg[3];
            board.addChecker(x,y,'black');
            alert("You lose!");
        }
        else if ('man_won' == msg[1]) {
            alert("You won!");
        }
    }

  }


  var ws = new WebSocket(baseUrl + '/new_game/' + color + '/' + level);
  ws.onopen = function() {
    state = 'connected';
  };
  ws.onmessage = function(e) {
    var msg = JSON.parse(e.data);
    handle_message(msg);
  };
  ws.onerror = function(err) {
    alert('ws error: ' + err);
  };
  this.ws = ws;

}

