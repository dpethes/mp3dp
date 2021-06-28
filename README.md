mp3dp - MP3 decoder for Pascal
===========

Decoder for MPEG layer 3 streams, usable for Freepascal projects. 
Derived from Delphi port of "maplay" version 1.2 (https://sourceforge.net/projects/delphimpeg/), which has some decoding bugs.
Changes:
* decoding bugfixes
* replaced player demo
* general code cleanup (a lot of unused/weird code, dating back to 90's C decoders)
* code size optimizations
* removed Layer 2 decoder code, MPEG2 LSF extension
* usable in 64bit projects

Usage and compilation
-----------

Use recent Lazarus with Freepascal (version 3.0 and higher) to compile.
Older versions may work, but are not recommended. Tested under Windows 32&64 bit,
but has no OS specific code so it'll most likely work anywhere.

There are 2 Lazarus project files that show the usage of the decoder:
* mp3_decode - decodes sample file to raw 16 bit signed PCM file (2 channels)
* mp3player_demo - decodes sample file and plays audio through SDL2 library, uses ImGui for display

Limitations
-----------
* assumes little-endian arch
* intensity stereo mode decoding is disabled (untested, might work)

Todo
-----------
* decode by frames/chunks to reduce required memory size
* mp3 file analysis to handle tags, seeking
* intensity stereo mode decoding - need samples
* reduce code size
* maybe re-add MPEG2 LSF extension handling, but needs samples

Thanks
-----------
* https://github.com/ZaneDubya/MP3Sharp
> Decode MP3 files to PCM bitstreams entirely in .NET managed code
* http://blog.bjrn.se/2008/10/lets-build-mp3-decoder.html
* https://www.bensound.com/royalty-free-music
> The sample MP3 file "Happy Rock" is from Bensound.com.