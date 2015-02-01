 Delphi MIDI I/O components               Version 7.0 28 May 2010
 --------------------------               ---------------------------

This release incorporates changes neccessary for use with newer
Delphi releases (developed under Delphi 2010 Professional).
Components should work in D6 and up.

It introduces new features to MidiIn/Out, DeviceID reverted to the
intended Cardinal handling as described by the Win32 SDK (MSDN) and
much more. I've also reformatted some of the code to meet modern
standards, moved the Examples to a new folder and created a new
package for visual use of the components. Components have been
tested to work on WinXP (32bit) so far.

Main intention was to have better MIDI I/O components for my Line6
project. Since the original source code was released as PUBLIC DOMAIN
I've changed the licensing to the MPL/GPL/LGPL to encourage further
development everyone can benefit from. Big THANKS to David Churcher.
His 13 years old components are the base for these improvements!
Thanks to turboPASCAL and FAlter for adding modern Delphi support.

Download & issue tracker can be found at
  http://bitbucket.org/h4ndy/midiio-dev
	
German discussion thread located at
  http://www.delphipraxis.net/topic178030,0,asc,0.html
	

- Manuel Kroeber <manuel.kroeber@googlemail.com>; Germany 2010


 Delphi MIDI I/O components               Version 6.1 16 Oct 2007
 --------------------------               ---------------------------

Version 6.1 (16. Oktober 2007)
Aktive Bereichsprüfung führte zu Exceptions - vermutlich verwendet die
MMSystem einen falschen Typ, denn Integer wäre logischer als Cardinal,
da -1 definitiv erlaubt ist (MIDI-Mapper).

--- English Translation by Manuel Kroeber/2010
Activated range checking is leading to exceptions. Maybe because the
MMSystem seems to use a wrong type, because Integer would be a more
logical choice than Cardinal, because -1 is allowed (MS MIDI Mapper).

v6.1 was done by FAlter, Member of DelphiPraxis.net board.


 Delphi MIDI I/O Components               Version 6.0 30. Juni 2005
 --------------------------               ---------------------------

Hi,

Dies ist NICHT das Originalpackage! Die Dateien wurden von turboPASCAL
aus der DP freundlicherweise verbessert.

Dieses Package hier wurde für Delphi 6 entwickelt.

Mfg
FAlter

--- English Translation by Manuel Kroeber/2010
This ISN'T the original package. The files have been improved by
turboPASCAL, member of DelphiPraxis.net board.

This package has been developed for use with Delphi 6.

Best regards,
FAlter (member of DelphiPraxis.net board)


--- Original ---
 Delphi MIDI I/O Components               Version 3.0c  14 Mar 2003
 --------------------------               -------------------------
 
 These components handle low-level MIDI input and output using the 
Windows multimedia MIDI functions. They encapsulate all the nasty
low-level stuff into some intermediate-level components. They support
both short MIDI messages (e.g. note on/off, program change) and long
MIDI messages (e.g. system exclusive, sample dumps).

 The components work under Delphi 3. If you need to use Delphi 1 or 2
you should get the previous version 2.0b. They've been tested in Windows
95 and NT 4.  

 To install the components, install the package MIDICOMP.DPL.
This should give you MIDI input and output components on the Samples tab.

 There's no formal documentation, but there are lists of properties,
methods, and events in the headers of MIDIIN.PAS and MIDIOUT.PAS.
There's also a couple of example projects: MIDIMON.DPR is a simple
monitor that demonstrates using components created at design time, and
MULTIMON.DPR demonstrates using multiple input and output components
created at runtime.

 These components are in the public domain so feel free to produce
any type of program based on them. 

 If you need to know more about MIDI see the links at the bottom of 
my MIDI page http://go.to/davesmidi.


Changes for Delphi 3:
---------------------

1. The requirement for a separate DLL for the MIDI callback procedure has
been removed. The callback is now in a fixed segment of the main
EXE.

2. Minor tweaks to compile without warnings under Delphi 3.

 I haven't really been developing these components very much as the
project I wrote them for never came about, so there are no changes
to functionality at all.


Frequently Asked Questions
--------------------------

I've had a lot of email about these components. Thanks to everyone
who sent kind words, code, and bug reports. 

 Q: How do you load MIDI files and play them using these components?

 A: These components don't do that, and the operating system only
    provides support for playing MIDI files from disk. You need to
    write your own code to load MIDI files. There are lots of C++
    examples around on the net, so get cracking!

 -------------
 Q: I don't want to do anything fancy, I just want to output some MIDI notes,
    how do I do that?

 A: These components were mainly built for doing System Exclusive input and
    output so if you don't want to use sysex, you don't really need these
    components to do that, although they make managing the device handles
    slightly easier.

    To output notes without the MidiOut component:
    ----------------------------------------------
     1. Open the device using an integer device ID to specify which
       device. In the example below devID is an integer from 0 to the no. of
       MIDI devices installed-1. You can get the number of installed devices
       from midiOutGetNumDevs and the names of the devices using
       midiOutGetDevCaps. 

       You can also use MIDI_MAPPER for the device ID, which directs output to
       the MIDI device configured in the Control Panel MIDI applet.
 
         var
            devHandle: HMIDIIN;
            midiRes: MMRESULT;
         begin
            { Callbacks not necessary for output }
            devHandle := midiOutOpen( @devHandle, devID, 0, 0, CALLBACK_NULL );

     2. Build up a 32-bit MIDI message value using the MIDI codes defined
        in the MIDI spec (see above for reference).

         For example:

         theMsg := DWORD(MidiMessage) Or
                (DWORD(MidiData1) shl 8) Or
                (DWORD(MidiData2) shl 16);


     3. Output the message using midiOutShortMsg with the handle from
        midiOutOpen.

            midiRes := midiOutShortMsg( devHandle, theMsg );

     4. Call midiOutClose() when you've finished.

    
    To output notes using the MidiOutput component:
    -----------------------------------------------

     1. Drop the component on a form.

     2. Set the component's DeviceID or DeviceName properties to set
        the output device, either manually or with code.
        Use the MidiMapper ID (-1) to output to the Windows MIDI Mapper.

     3. Call MidiOutput.Open.

     4. Call Midioutput.PutShort(MidiMessage, Data1, Data2).

     5. Call MidiOutput.Close when you've finished.

 -------------
 Q: How do I construct and send system exclusive (Sysex) data?

 A: It's easiest to work with the data in strings. You can build
literal strings like this:

  sMidi :=  #$F0#$47#00#$28#$48#00#00#00#$0F#01#00#$0F#03#$F7;

Use Chr() to add variables to the sequence, e.g.
 	sSysex := #$f0 + Chr(channel) + Chr(address) + ... + #$f7;

 A common mistake people make is to try to send ASCII hex data like this:

  sMidi := 'F001000313F7';  { Wrong! }

 There are two ways of getting a pointer to the string to pass to
TMidiOutput.Putlong:

     i) In Delphi 2+, cast the string to a pointer, e.g.

  MidiOutput1.PutLong(Pointer(sMidi), Length(sMidi));

     ii) In Delphi 1, use @sMidi[1] to skip past the length byte at the
start of the string, e.g.

  MidiOutput1.PutLong(@sMidi[1], Length(sMidi));

 Do not use any of the zero-terminated string functions Str*() on sysex data
as sysex sequences often contain zeros and will be truncated. The exception
is the StrMove() function which doesn't stop for binary zeros.

 -------------
 Q: How do I receive MIDI timing data?

 A: This is filtered out by default in the callback function in DELPHMCB.PAS.
Enable it by removing the check for MIDI_TIMINGCLOCK in the midiHandler function.

 Contacting me
 -------------

 Contact me by email at dchurcher@cix.co.uk, but please remember that you
got the components for free and I don't get paid for support.

 Updated versions of this component may appear on my MIDI software web
page:

    http://go.to/davesmidi



 Revision history
 -----------
v3.0c
-----
Bug fix: TMidiIn would prevent Windows shutdown by not responding
to WM_QUERYENDSESSION message.


v3.0b
-----
Bug fix: With some MIDI drivers sysex data would be added on to the end
of input buffers after the first set of buffers was used up. Thanks to 
Pete from Soundcraft for fixing this.

README.TXT updated with a more mobile web page.


v3.0
----
1. Upgraded to Delphi 3

2. Removed requirement for DLL.


v2.0b
-----
1. Added 16-bit DCR files in 16bitres.zip

2. Removed "uses midioutTimerHandler" from delphmid.dpr


v2.0
----
1. Fixed exception on load if the installed MIDI devices were different
from the development machine.

2. Now throws an "Invalid device ID" exception on Create if no MIDI
devices installed on machine.

3. Fixed GPF when SysexBufferCount = 0


 David Churcher
 June 1997
