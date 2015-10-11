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
You should have UART Port connected to a sensor and mongooseIM configured
if not change line in iot.app.src " {env,[{mocked,true}]} " to  {env,[{mocked,false}]}

    $ sh start.sh


