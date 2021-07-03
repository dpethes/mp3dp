mp3dp - MP3 decoder for Pascal
===========

Decoder for MPEG layer 3 streams, usable for Freepascal projects. Compiles to about 35kB of code on x86.

Usage and compilation
-----------
Use recent Lazarus with Freepascal (version 3.0 and higher) to compile.
Older versions may work, but are not recommended. Tested under Windows 32&64 bit,
but has no OS specific code so it'll most likely work anywhere.

There are 2 Lazarus project files that show the usage of the decoder:
* mp3_decode - decodes sample file to raw 16 bit signed PCM file (2 channels)
* mp3player_demo - decodes sample file and plays audio through SDL2 library, uses ImGui for display

History
-----------
The project started by trying to fix decoding errors in the Delphi port of "maplay".
There were some occasional audible errors in its output, so I started trimming down the code to narrow down the cause. 
I got more interested in whole mp3 format and its details in the process.
Eventually, bugs got fixed, but I kept on fiddling with the code, rewriting chunks of it
(a lot of unused/weird code, dating back to 90's C decoders),
doing memory usage and code size optimizations, making the code work on 64bit and replacing the sample player.
The result should be a small mp3 decoder that can be included in other projects easily.

Limitations
-----------
* assumes little-endian arch
* doesn't support MPEG2 LSF extension
* doesn't decode Layer 1&2 (nobody misses it)
* doesn't decode free format bitstream (very rare?)
* no intensity stereo mode decoding (super rare or non-existent in the wild)
* output of x87 vs. SSE FPU mode is not bitexact (should not bother you)

Todo
-----------
* decode by frames/chunks to reduce required memory size
* mp3 file analysis to handle tags, seeking, free format bitstream
* parse VBR, lame info tags
* table based vlc decoding
* reduce code size, both source and binary
* re-add MPEG2 LSF extension handling
* add intensity stereo mode decoding under $define, just for the sake of completeness?
* compliance check

Resources & Thanks
-----------
* https://sourceforge.net/projects/delphimpeg/
  > Delphi port of "maplay" version 1.2
* https://github.com/ZaneDubya/MP3Sharp
  > Decode MP3 files to PCM bitstreams entirely in .NET managed code
* http://blog.bjrn.se/2008/10/lets-build-mp3-decoder.html
  > Good description of mp3 decoding process.
* http://keyj.emphy.de/minimp3/ , https://keyj.emphy.de/kjmp2/
  > Small mp2/mp3 decoders with some interesting details in blogposts.
* https://github.com/lieff/minimp3
  > Minimalistic MP3 decoder single header library 
* https://www.bensound.com/royalty-free-music
  > The sample MP3 file "Happy Rock" is from Bensound.com.


