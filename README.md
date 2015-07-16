iot
=====

An OTP application

Build
-----
    $ rebar3 compile

Start Application
-----
    (Mac,Unix)
    $ sh start.sh

Run Tests
----
    rebar3 ct

Create Release
----
    rebar3 release

Example Usage
----
You should have UART Port connected (and loopbacked)
if not change line in iot.app.src " {env,[{mocked,true}]} " to  {env,[{mocked,false}]}

    $ sh start.sh
    (IoF@127.0.0.1)> var_server:openport().
    {ok,<0.XY.0>}
    (IoF@127.0.0.1)> var_server:getdata().
    []
    (IoF@127.0.0.1)> driver_server:senddata("Some text").
    ok
    (IoF@127.0.0.1)> var_server:getdata().
    [{#Port<0.XYZ>,{data,<<"Some text">>}}]
    (IoF@127.0.0.1)>  driver_server:senddata("More text").
    ok
    (IoF@127.0.0.1)> var_server:getdata().
    [{#Port<0.XYZ>,{data,<<"Some text">>}},
     {#Port<0.XYZ>,{data,<<"More text">>}}]


