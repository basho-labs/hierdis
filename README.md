## Overview

High-performance Erlang client for the Redis key-value store (NIF wrapping the hiredis C client).


## Installation

**Pre-requisites:** You must already have Erlang R15B01 or later installed on your machine.

	$ git clone git@github.com:nathanaschbacher/hierdis.git .
	$ cd hierdis
	$ ./rebar compile
	
This should automatically build the `hiredis` dependencies and move the headers and libraries to the `priv/` directory.


## Usage

    $ erl -pa ebin/
    
    Eshell V5.9.1  (abort with ^G)
    1> 
    
    2> 
    
    3> 
    


## License

(The MIT License)

Copyright (c) 2013 Nathan Aschbacher

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
'Software'), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.