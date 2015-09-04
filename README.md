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
    (IoF@127.0.0.1)> driver_manager:open_port(driver,uart).
    {ok,<0.XY.0>}
    (IoF@127.0.0.1)> driver_manager:get_data().
    []
    (IoF@127.0.0.1)> driver_manager:send_data(driver, "Some text").
    ok
    (IoF@127.0.0.1)> driver_manager:get_data().
    [{#Port<0.XYZ>,{data,<<"Some text">>}}]
    (IoF@127.0.0.1)>  driver_manager:send_data(driver, "More text").
    ok
    (IoF@127.0.0.1)> driver_manager:get_data().
    [{#Port<0.XYZ>,{data,<<"Some text">>}},
     {#Port<0.XYZ>,{data,<<"More text">>}}]

Bugs
----
    Yet dont know why you have to start driver at least two times to get driver working.
    Working on it

