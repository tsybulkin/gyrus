function Board(parent, cellSize, callback) {

    var self = this;
    var size = 14;
    active = typeof active !== 'undefined' ? active : false;
    // Mouse coordinates
    var x = 0, y = 0;

    this.clear = function () {
        self.matrix = self._emptyMatrix()
        var circles = self.div.getElementsByClassName('circle');
        for (var i = circles.length; i--;) {
            circles[i].remove();
        }
    };

    // create background dom element
    var background = document.createElement('div');
    background.className = 'board_background';
    parent.appendChild(background);

    // create parent dom element
    var board = document.createElement('div');
    board.className = 'board';
    background.appendChild(board);
    this.div = board;

    var table = document.createElement('table');
    table.className = 'board_table';
    board.appendChild(table);

    var thinking = document.createElement('div');
    thinking.className = 'thinking';
    thinking.innerHTML = 'Your opponent\'s ply...';
    board.appendChild(thinking);
    this.thinking = function () {
        thinking.style.visibility = 'visible';
    }
    this.not_thinking = function () {
        thinking.style.visibility = 'hidden';
    }

    var message = document.createElement('div');
    message.className = 'message';
    board.appendChild(message);
    this.message = function (text) {
        message.innerHTML = text;
        thinking.style.visibility = 'hidden';
        message.style.visibility = 'visible';
    }

    // add rows
    _.times(size, function (n) {
        var row = document.createElement("tr");
        table.appendChild(row);

        // add cells
        _.times(size, function (n) {
            var cell = document.createElement("td");
            cell.style.width = cellSize + 'px';
            cell.style.height = cellSize + 'px';
            row.appendChild(cell);
        });

    });

    this.addChecker = function (x, y, color, classes) {
        classes = typeof classes !== 'undefined' ? classes : ' ';
        var ch = document.createElement('div');
        ch.id = x + '_' + y;
        ch.className = 'circle' + classes + color;
        ch.style.width = cellSize - 4 + 'px';
        ch.style.height = cellSize - 4 + 'px';
        ch.style.position = 'absolute';
        var cell_size = cellSize;
        var ch_size = cellSize;
        ch.style.left = cell_size * (x - 1) - ch_size / 2 + x + 'px';
        ch.style.bottom = cell_size * (y - 1) - ch_size / 2 + y + 'px';
        board.appendChild(ch);
        self.matrix[x - 1][y - 1] = true;
    };

    this.winChecker = function (x, y, color) {
        if (self.matrix[x - 1][y - 1]) {
            var ch = self.findChildById(board, x + '_' + y);
            ch.className += ' win';
        }
        else {
            self.addChecker(x, y, color, ' win ');
        }
    };

    if (callback) {
        var ch = document.createElement('div');
        ch.className = 'circle next';
        ch.style.width = cellSize - 4 + 'px';
        ch.style.height = cellSize - 4 + 'px';
        ch.style.position = 'absolute';
        board.appendChild(ch);
        var next_checker = ch;

        function _handleMouseMove(event) {
            x = Math.round(event.layerX / (cellSize + 1)) + 1;
            y = 15 - Math.round(event.layerY / (cellSize + 1));

            next_checker.style.left = Math.round(event.layerX / (cellSize + 1)) * (cellSize + 1) - cellSize / 2 + 1 + 'px';
            next_checker.style.top = Math.round(event.layerY / (cellSize + 1)) * (cellSize + 1) - cellSize / 2 + 'px';

        }

        function _handleClick() {
            if (!self.matrix[x - 1][y - 1]) {
                callback(x, y);
            }
        }

        table.onmousemove = _handleMouseMove;
        board.onclick = _handleClick;
    }

    this._emptyMatrix = function () {
        return _.range(size + 1).map(function (n) {
            return _.range(size + 1).map(function (n) {
                return false;
            });
        });
    }

    this.matrix = self._emptyMatrix();

    this.findChildById = function (element, childID) {
        var retElement = null;
        var lstChildren = element.childNodes;

        for (var i = 0; i < lstChildren.length; i++) {
            if (lstChildren[i].id == childID) {
                retElement = lstChildren[i];
                break;
            }
        }

        return retElement;
    }

}

function BoardConnection(board, baseUrl, color, level) {
    var state = 'init';
    var self = this;
    var bots_color = (color == 'whites') ? 'blacks' : 'whites';

    this.send = function (obj) {
        var json = JSON.stringify(obj);
        self.ws.send(json);
    }

    function handle_message(msg) {
        var action = msg[0];
        if ('bot_move' == action) {
            board.not_thinking();
            var x = msg[1];
            var y = msg[2];
            board.addChecker(x, y, bots_color);
        }
        else if ('game_over' == action) {
            console.log(msg);
            if ('man_lost' == msg[1]) {
                var x_indices = _.range(2, 12, 2);
                x_indices.forEach(function (x_index) {
                    var x = msg[x_index];
                    var y = msg[x_index + 1];
                    board.winChecker(x, y, bots_color);
                });
                board.message("You lose!");
            }
            else if ('man_won' == msg[1]) {
                var x_indices = _.range(2, 12, 2);
                x_indices.forEach(function (x_index) {
                    var x = msg[x_index];
                    var y = msg[x_index + 1];
                    board.winChecker(x, y, color);
                });
                board.message("You won!");
            }
        }
        else if ('counters' == action) {
            var won = msg[1];
            var draw = msg[2];
            var lose = msg[3];
            var done = msg[4];

            document.getElementById('won').innerHTML = won;
            document.getElementById('draw').innerHTML = draw;
            document.getElementById('lose').innerHTML = lose;
            document.getElementById('done').innerHTML = done;
        }

    }

    var ws = new WebSocket(baseUrl + '/new_game/' + color + '/' + level);
    ws.onopen = function () {
        state = 'connected';
    };
    ws.onmessage = function (e) {
        var msg = JSON.parse(e.data);
        handle_message(msg);
    };
    ws.onerror = function (err) {
        alert('ws error: ' + err);
        console.log('ws error: ' + err);
    };
    this.ws = ws;

    this.close = function () {
        ws.close();
    }
}

function DemoBoardConnection(board, baseUrl) {
    var self = this;

    function handle_message(msg) {
        var action = msg[0];
        if ('demo_game_move' == action) {
            var color = msg[1];
            var x = msg[2];
            var y = msg[3];
            board.addChecker(x, y, color);
        }
        else if ('demo_game_board' == action) {
            var m = msg[1];
            for (var x = 0; x < 15; x++)
                for (var y = 0; y < 15; y++) {
                    var c = m[y][x];
                    if ('w' == c)
                        board.addChecker(x + 1, y + 1, 'whites');
                    else if ('b' == c)
                        board.addChecker(x + 1, y + 1, 'blacks');
                }
        }
        else if ('demo_game_over' == action && msg[1] != 'draw') {
            console.log(msg);
            var color = msg[1];
            var x_indices = _.range(2, 12, 2);
            console.log(msg, x_indices);
            x_indices.forEach(function (x_index) {

                console.log(x_index);
                var x = msg[x_index];
                var y = msg[x_index + 1];
                board.winChecker(x, y, color);
            });
            board.clear();
        }

    }

    var ws = new WebSocket(baseUrl + 'demo_game');
    ws.onmessage = function (e) {
        var msg = JSON.parse(e.data);
        handle_message(msg);
    };
    ws.onerror = function (err) {
        alert('ws error: ' + err);
        console.log('ws error: ' + err);
    };
    this.ws = ws;

    this.close = function () {
        ws.close();
    }
}

