0.5.4 (12/02/2022)
==================
* Update package dependency constraints
    * base >= 4.12 && < 4.17

0.5.3 (26/09/2021)
==================
* Update package dependency constraints
    * Win32 >= 2.11 && < 2.14
* Add documentation and code samples

0.5.2 (30/3/2021)
=================
* Update package dependency constraints

0.5.1 (16/1/2021)
=================
* Lock Posix serial port for exclusive access
* Simplify internal usage of serial port handles under both Posix and Windows
* Add hWithSerial
* Minor updates to make future refactoring easier

0.5.0 (16/12/2020)
==================
* Derive Show and Read instances for SerialPortSettings, CommSpeed, StopBits, Parity, and FlowControl datatypes.
* Update minimum Cabal-version to 1.10
* Minor syntax changes
* Minor code cleanup

0.4.7 (28/08/2014)
================

* Open in non-blocking mode, immediately reverting to blocking to fix OS-X problem
