%% -*- erlang -*-
%%   FFE file words
%%
-ifndef(__FILE__).
-define(__FILE__, true).

-record(posix_error,
	{
	 eacces, %% Permission denied
	 eagain, %% Resource temporarily unavailable
	 ebadf, %% Bad file number
	 ebusy, %% File busy
	 edquot, %% Disk quota exceeded
	 eexist, %% File already exists
	 efault, %% Bad address in system call argument
	 efbig, %% File too large
	 eintr, %% Interrupted system call
	 einval, %% Invalid argument
	 eio, %% I/O error
	 eisdir, %% Illegal operation on a directory
	 eloop, %% Too many levels of symbolic links
	 emfile, %% Too many open files
	 emlink, %% Too many links
	 enametoolong, %% Filename too long
	 enfile, %% File table overflow
	 enodev, %% No such device
	 enoent, %% No such file or directory
	 enomem, %% Not enough memory
	 enospc, %% No space left on device
	 enotblk, %% Block device required
	 enotdir, %% Not a directory
	 enotsup, %% Operation not supported
	 enxio, %% No such device or address
	 eperm, %% Not owner
	 epipe, %% Broken pipe
	 erofs, %% Read-only file system
	 espipe, %% Invalid seek
	 esrch, %% No such process
	 estale, %% Stale remote file handle
	 exdev  %% Cross-domain link
	}).

-define(BINARY, 16#8).
-define(READ,   16#4).
-define(WRITE,  16#2).
-define(EXEC,   16#1).

file_words() ->
    #{
      ?WORD("close-file",   close_file),
      ?WORD("create-file",  create_file),
      ?WORD("delete-file",  delete_file),
      ?WORD("flush-file",   flush_file),
      ?WORD("open-file",    open_file),
      ?WORD("rename-file",  rename_file),
      ?WORD("resize-file",  resize_file),
      ?WORD("include-file", include_file),
      ?WORD("included",     included),
      ?WORD("include",      include),
      ?WORD("read-file",    read_file),
      ?WORD("read-line",    read_line),
%%    ?WORD("refill",       refill),   CORE+FILE
      ?WORD("write-file",   write_file),
      ?WORD("write-line",   write_line),
      ?WORD("bin",          bin),
      ?WORD("file-position",file_position),
      ?WORD("file-size",    file_size),
      ?WORD("file-status",  file_status),
      ?WORD("r/o",          read_only),
      ?WORD("r/w",          read_write),
      ?WORD("reposition-file", reposition_file),
%%    ?WORD("s\"",          s_quote),  CORE+FILE
      ?WORD("w/o",          write_only)
     }.

%% create new fileid
new_fileid() ->
    FileID = case get(next_fileid) of
		 undefined -> 3;
		 NextFileID -> NextFileID
	     end,
    put(next_fileid, FileID+1),
    FileID.

put_fd(FileID, Fd) ->
    put({fileid,FileID}, Fd).

get_fd(FileID) ->
    get({fileid,FileID}).

del_fd(FileID) ->
    %% fixme: reuse FileID
    erase({fileid,FileID}).
    

?XT("r/o", read_only).
read_only(SP,RP,IP,WP) ->
    next([?READ|SP],RP,IP,WP).

?XT("w/o", write_only).
write_only(SP,RP,IP,WP) ->
    next([?WRITE|SP],RP,IP,WP).

?XT("r/w", read_write).
read_write(SP,RP,IP,WP) ->
    next([(?READ bor ?WRITE)|SP],RP,IP,WP).

?XT("bin", bin).
bin([AccessMode|SP],RP,IP,WP) ->
    next([(?BINARY bor AccessMode)|SP],RP,IP,WP).

?XT("close-file", close_file).
close_file([FileID|SP],RP,IP,WP) ->
    IOR = file_close(FileID),
    next([IOR|SP],RP,IP,WP).

?XT("create-file", create_file).
create_file([Fam,N,A|SP],RP,IP,WP) ->
    Read  = Fam band ?READ =:= ?READ,
    Write = Fam band ?WRITE =:= ?WRITE,
    %% first create the file
    <<Filename:N/binary, _/binary>> = A,
    Mode0 = [read,write,raw,binary],
    case file:open(Filename, Mode0) of
	{ok,Fd} ->
	    if Read, Write ->
		    file:truncate(Fd),
		    FileID = new_fileid(),
		    put_fd(FileID, Fd),
		    next([0,FileID|SP],RP,IP,WP);
	       true ->
		    file:truncate(Fd),
		    file:close(Fd),
		    Mode = if Read -> [read,raw,binary];
			      Write -> [write,raw,binary];
			      true -> [read,raw,binary]  %% ? default?
			   end,
		    [IOR,Fd1] = file_open(Filename, Mode),
		    next([IOR,Fd1|SP],RP,IP,WP)
	    end;
	{error,Reason} ->
	    next([ior(Reason),0|SP],RP,IP,WP)
    end.

?XT("open-file", open_file).
open_file([Fam,N,A|SP],RP,IP,WP) ->
    Read  = Fam band ?READ =:= ?READ,
    Write = Fam band ?WRITE =:= ?WRITE,
    %% first create the file
    <<Filename:N/binary, _/binary>> = A,
    %% Note that forth open-file should not create file, therefor
    %% we try open the file in read only mode and then open in read/write
    %% if needed
    Mode0 = [read,raw,binary],
    case file:open(Filename, Mode0) of
	{ok,Fd} ->
	    if not Write ->
		    FileID = new_fileid(),
		    put_fd(FileID, Fd),
		    next([0,FileID|SP],RP,IP,WP);
	       true ->
		    Mode = if Read -> [read,write,raw,binary];
			      true -> [write,raw,binary]
			   end,
		    file:close(Fd),
		    [IOR,Fd1] = file_open(Filename, Mode),
		    next([IOR,Fd1|SP],RP,IP,WP)
	    end;
	{error,Reason} ->
	    next([ior(Reason),0|SP],RP,IP,WP)
    end.

?XT("delete-file", delete_file).
delete_file([Name,Len|SP],RP,IP,WP) ->
    <<Filename:Len/binary, _/binary>> = Name,
    case file:delete(Filename) of
	ok ->
	    next([0|SP],RP,IP,WP);
	{error,Reason} ->
	    next([ior(Reason)|SP],RP,IP,WP)
    end.

?XT("write-file", write_file).
write_file([FileID,U,Addr|SP],RP,IP,WP) ->
    <<Data:U/binary,_>> = Addr,
    case file:write(get_fd(FileID),Data) of
	ok ->
	    next([0|SP],RP,IP,WP);
	{error,Reason} ->
	    next([ior(Reason)|SP],RP,IP,WP)
    end.

?XT("read-file", read_file).
read_file([FileID,U,Addr|SP],RP,IP,WP) ->
    <<Data:U/binary,_>> = Addr,
    case file:read(get_fd(FileID),U) of
	{ok,Data} ->
	    %% where to put Data? (at Addr?)
	    next([byte_size(Data),0|SP],RP,IP,WP);
	{error,Reason} ->
	    next([ior(Reason)|SP],RP,IP,WP)
    end.

?XT("file-position", file_position).
file_position([FileID|SP],RP,IP,WP) ->
    case file:position(get_fd(FileID), {cur,0}) of
	{ok, Pos} ->
	    next([0,Pos|SP],RP,IP,WP);
	{error,Reason} ->
	    next([ior(Reason),0|SP],RP,IP,WP)
    end.

?XT("file-size", file_size).
file_size([FileID|SP],RP,IP,WP) ->
    Fd  = get_fd(FileID),
    Cur = file:position(Fd, {cur,0}),
    case file:position(Fd, {eof,0}) of
	{ok, Size} ->
	    file:position(Fd, Cur),  %% restore position
	    next([0,Size|SP],RP,IP,WP);
	{error,Reason} ->
	    next([ior(Reason),0|SP],RP,IP,WP)
    end.

?XT("read-info", file_info).
file_info([N,A|SP],RP,IP,WP) ->
    <<Filename:N/binary, _/binary>> = A,
    case file:read_file_info(Filename) of
	{ok,Info} ->
	    %% where to put Data? (at Addr?)
	    next([0,Info|SP],RP,IP,WP);
	{error,Reason} ->
	    next([ior(Reason),0|SP],RP,IP,WP)
    end.

?XT("write-line", write_line).
write_line([FileID,U,Addr|SP],RP,IP,WP) ->
    <<Data:U/binary,_>> = Addr,
    case file:write(get_fd(FileID),[Data,?NL]) of
	ok ->
	    next([0|SP],RP,IP,WP);
	{error,Reason} ->
	    next([ior(Reason)|SP],RP,IP,WP)
    end.

%% internal file open / close
file_open(Filename, Mode) ->
    case file:open(Filename, Mode) of
	{ok,Fd} ->
	    FileID = new_fileid(),
	    put_fd(FileID, Fd),
	    [0,FileID];
	{error,Reason} ->
	    [ior(Reason), 0]
    end.

file_close(Fd) ->
    case file:close(get_fd(Fd)) of
	ok ->
	    del_fd(Fd),    
	    0;
	{error,Reason} ->
	    del_fd(Fd),
	    ior(Reason)
    end.


ior(Reason) ->
    case Reason of
	ok     -> 0;
	eacces -> #posix_error.eacces;
	eagain -> #posix_error.eagain;
	ebadf -> #posix_error.ebadf;
	ebusy -> #posix_error.ebusy;
	edquot -> #posix_error.edquot;
	eexist -> #posix_error.eexist;
	efault -> #posix_error.efault;
	efbig -> #posix_error.efbig;
	eintr -> #posix_error.eintr;
	einval -> #posix_error.einval;
	eio -> #posix_error.eio;
	eisdir -> #posix_error.eisdir;
	eloop -> #posix_error.eloop;
	emfile -> #posix_error.emfile;
	emlink -> #posix_error.emlink;
	enametoolong -> #posix_error.enametoolong;
	enfile -> #posix_error.enfile;
	enodev -> #posix_error.enodev;
	enoent -> #posix_error.enoent;
	enomem -> #posix_error.enomem;
	enospc -> #posix_error.enospc;
	enotblk -> #posix_error.enotblk;
	enotdir -> #posix_error.enotdir;
	enotsup -> #posix_error.enotsup;
	enxio -> #posix_error.enxio;
	eperm -> #posix_error.eperm;
	epipe -> #posix_error.epipe;
	erofs -> #posix_error.erofs;
	espipe -> #posix_error.espipe;
	esrch -> #posix_error.esrch;
	estale -> #posix_error.estale;
	exdev  -> #posix_error.exdev;
	_ -> -1
    end.

error_string(Err) ->
    case Err of
	0 -> "ok";
	-1 -> "Unknown error";
	#posix_error.eacces -> "Permission denied";
	#posix_error.eagain -> "Resource temporarily unavailable";
	#posix_error.ebadf -> "Bad file number";
	#posix_error.ebusy -> "File busy";
	#posix_error.edquot -> "Disk quota exceeded";
	#posix_error.eexist -> "File already exists";
	#posix_error.efault -> "Bad address in system call argument";
	#posix_error.efbig -> "File too large";
	#posix_error.eintr -> "Interrupted system call";
	#posix_error.einval -> "Invalid argument";
	#posix_error.eio -> "I/O error";
	#posix_error.eisdir -> "Illegal operation on a directory";
	#posix_error.eloop -> "Too many levels of symbolic links";
	#posix_error.emfile -> "Too many open files";
	#posix_error.emlink -> "Too many links";
	#posix_error.enametoolong -> "Filename too long";
	#posix_error.enfile -> "File table overflow";
	#posix_error.enodev -> "No such device";
	#posix_error.enoent -> "No such file or directory";
	#posix_error.enomem -> "Not enough memory";
	#posix_error.enospc -> "No space left on device";
	#posix_error.enotblk -> "Block device required";
	#posix_error.enotdir -> "Not a directory";
	#posix_error.enotsup -> "Operation not supported";
	#posix_error.enxio -> "No such device or address";
	#posix_error.eperm -> "Not owner";
	#posix_error.epipe -> "Broken pipe";
	#posix_error.erofs -> "Read-only file system";
	#posix_error.espipe -> "Invalid seek";
	#posix_error.esrch -> "No such process";
	#posix_error.estale -> "Stale remote file handle";
	#posix_error.exdev -> "Cross-domain link"
    end.
      
-endif.

