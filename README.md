hidmacros
=========
These files are sources of HidMacros application - http://www.hidmacros.eu.
As author of this application has no time for further development source code is now available for everyone.
The application is developped in TurboDelphi as windows 32bit application. It consist of 3 main projects:
* HidMacros application itself (GUI)
* WinHook dll - dynamic library for windows global hooking, to block key press for defined macro in target application
* Xplane plugin - dll for Xplane interface

As Xplane 10 is mainfly 64b application and TurboDelphi is not free with 64b compiler the Xplane plugin was also compiled in Lazarus using free pascal compiler. It has separate folder with sources however it's copy of whole application with only few changes for compilation.
The code structure and organization is not perfect now so some cleanup and documentation is one of first tasks to be done. However it works and those are real sources used for last build.
The application has several dependencies (DirectX API, SimConnect SDK, X-plane SDK) - those are not included but should be public available.
However if anyone has difficulties to compile this application, please contact author at admin@hidmacros.eu.

This readme file contains only basic info and should be improved.
