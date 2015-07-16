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
 (You have to have UART Port (and loopbacked)
if not change line in iot.app.src " {env,[{mocked,true}]} " to  {env,[{mocked,false}]}

    $ sh start.sh
    erl> var_server:openport().
    {ok,<0.XY.0>}
    erl> var_server:getdata().
    []
    erl> driver_server:senddata("Some text").
    ok
    erl> var_server:getdata().
    [{#Port<0.XYZ>,{data,<<"Some text">>}}]
    erl> driver_server:senddata("More text").
    ok
    erl> var_server:getdata().
    [{#Port<0.XYZ>,{data,<<"Some text">>}},
     {#Port<0.XYZ>,{data,<<"More text">>}}]


