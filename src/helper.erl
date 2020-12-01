-module(helper).

-export([
    load_input/1,
    load_input_lines/1
]).

load_input(FileName)
->
    FilePath = <<"../inputs/", FileName/binary>>,
    {ok, FileBinary} = file:read_file(FilePath),
    {ok, FileBinary}
.

load_input_lines(FileName)
->
    {ok, FileBinary} = load_input(FileName),
    FileLines = binary:split(FileBinary, [<<"\r">>, <<"\n">>, <<"\r\n">>], [global]),
    {ok, FileLines}
.
