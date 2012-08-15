# erl_chan
*An implementation of a 4-chan style message board in Erlang/OTP*

## Project Goal

Build a fairly simple-to-deploy, robust, scalable and secure anonymous image board.

## Feature Roadmap

- comment markup
- spoiler/nsfw images
- stickied threads
- per-board custom CSS
- moderation audit system

See notes.org for a complete TODO list so far. Also, feel free to contribute.

## Usage

#### Dependencies

Nitrochan depends on

###### Programs

- [GNU Make](http://www.gnu.org/software/make/)
- [Erlang](http://www.erlang.org/)
- [GNU Screen](http://www.gnu.org/software/screen/)
- [Python 2.x](http://www.python.org/download/releases/2.7.2/)
- [imagemagick](http://www.imagemagick.org/script/index.php)
- [git](http://git-scm.com/)
- [Erlport](http://erlport.org/)

###### Libraries

- [Nitrogen](http://nitrogenproject.com/)
- [auth](https://github.com/Inaimathi/auth)
- [common](https://github.com/Inaimathi/common)
- [erlsha2](https://github.com/vinoski/erlsha2)
- [m2crypto](http://chandlerproject.org/Projects/MeTooCrypto)

#### Installation

If you've got GNU make and `git` installed, just run

    make install
    make mnesia-create
    
Starting the application shoulr be fairly simple once its been installed; just run

    make start
    
That will create two detached screen instances; one running the `nitrogen` framework with `yaws`, and the other running the `erl_chan` backend.

## Components

### board

Just one non-nitrogen component at the moment; a template for the board process.

    new/1, new/2, new_special/2
    
These functions create new boards. `new_special` creates an internal board (like the default `admin` board) with a particularly named user group. `new/2` takes a name and description and creates a regular board (like the default `general` board). `new/1` is the equivalent of calling `new(BoardName, "").`.

    new_thread/2, reply/3
    
These two functions create comments (the first one also creates a new thread to store said comment).

    list/0, thread_meta/1, board_meta/1
    
`list/0` returns the complete list of all boards on the current node. `thread_meta/1` returns metadata about a given thread, and `board_meta/1` does the same for the given board.

    filter/2
    
This is a function meant to filter incoming comment responses. It takes a list of `CommentId`s, check that each one exists and returns a set (actually a list generated using `sets:to_list/1`) of `{ThreadId, CommentId}`.

    summarize/1, get_thread/1

`summarize/1` summarizes either a board or a thread. The summary of a `board` is a list of `thread` summaries, the summary of a `thread` is a preview of the first comment, along with a preview of up to the last 4 comments (fewer if there are fewer than 5 comments in the thread). `get_thread/1` returns a list of all comments (not just the previews) in the given thread.

    move/2, delete/2, revive/2, purge/2
    
`move/2` moves a thread to a new board. `delete/2` hides a given thread/comment/image, `revive/2` undoes the effects of a `delete/2`. `purge/2` does an actual deletion, removing the target thread/comment/image from the database entirely (in each case, it returns a list of image files associated with the database elements so that the front-end can delete them from disk).

## License

This program is released for general consumption under the AGPL3. The full license text can be found in LICENSE.txt or at [the GNU license page](http://www.gnu.org/licenses/agpl.html)

    Copyright © 2012  Inaimathi

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

Short version (totally not legally binding, read the full document for the legalese, this is just for your understanding):

- You may use this free of charge, with no warrany, in any way you please
- If you distribute it or a derivative work, you must grant all your users the same freedoms (along with the source code)
- If you set it or a derivative work up as a web service, you must grant all your users the same freedoms (along with the source code)
- Any code you add to this project must be licensed under the AGPL3, or a compatible license
