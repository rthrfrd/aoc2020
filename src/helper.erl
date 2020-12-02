-module(helper).

-export([
    get_input_path/1,
    load_input/1,
    load_input_lines/1
]).

get_input_path(FileName)
->
    <<"../inputs/", FileName/binary>>
.

load_input(FileName)
->
    FilePath = get_input_path(FileName),
    {ok, FileBinary} = file:read_file(FilePath),
    {ok, FileBinary}
.

load_input_lines(FileName)
->
    {ok, FileBinary} = load_input(FileName),
    FileLines = binary:split(FileBinary, [<<"\r">>, <<"\n">>, <<"\r\n">>], [global]),
    {ok, FileLines}
.
