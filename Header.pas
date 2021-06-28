(*
 *  File:     $RCSfile: Header.pas,v $
 *  Revision: $Revision: 1.1.1.1 $
 *  Version : $Id: Header.pas,v 1.1.1.1 2002/04/21 12:57:16 fobmagog Exp $
 *  Author:   $Author: fobmagog $
 *  Homepage: http://delphimpeg.sourceforge.net/
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *)
unit Header;
{$mode delphi}

interface
uses
  CRC, BitStream, BitReserve;

type
  TVersion         = (MPEG2_LSF, MPEG1);
  TMode            = (Stereo, JointStereo, DualChannel, SingleChannel);
  TSampleFrequency = (SampleFreq_44p1, SampleFreq_48, SampleFreq_32, Unknown);

type
  // Class for extraction information from a frame header:
  THeader = class
  private
    FLayer: Cardinal;
    FProtectionBit: Cardinal;
    FBitrateIndex: Cardinal;
    FPaddingBit: Cardinal;
    FModeExtension: Cardinal;
    FVersion: TVersion;
    FMode: TMode;
    FSampleFrequency: TSampleFrequency;
    FNumberOfSubbands: Cardinal;
    FIntensityStereoBound: Cardinal;
    FCopyright: Boolean;
    FOriginal: Boolean;
    FInitialSync: Boolean;
    FCRC: TCRC16;
    FOffset: PCardinalArray;
    FChecksum: Cardinal;
    FFrameSize: Cardinal;
    FNumSlots: Cardinal;

    function GetFrequency: Cardinal;
    function GetChecksums: Boolean;
    function GetChecksumOK: Boolean;
    function GetPadding: Boolean;

  public
    property Version: TVersion read FVersion;
    property Layer: Cardinal read FLayer;
    property BitrateIndex: Cardinal read FBitrateIndex;
    property SampleFrequency: TSampleFrequency read FSampleFrequency;
    property Frequency: Cardinal read GetFrequency;
    property Mode: TMode read FMode;
    property Checksums: Boolean read GetChecksums;
    property Copyright: Boolean read FCopyright;
    property Original: Boolean read FOriginal;
    property ChecksumOK: Boolean read GetChecksumOK;
    // compares computed checksum with stream checksum
    property Padding: Boolean read GetPadding;
    property Slots: Cardinal read FNumSlots;
    property ModeExtension: Cardinal read FModeExtension;
    property NumberOfSubbands: Cardinal read FNumberOfSubbands;
    // returns the number of subbands in the current frame
    property IntensityStereoBound: Cardinal read FIntensityStereoBound;
    // (Layer II joint stereo only)
    // returns the number of subbands which are in stereo mode,
    // subbands above that limit are in intensity stereo mode

    constructor Create;
    destructor Destroy; override;

    function ReadHeader(Stream: TBitStream; var CRC: TCRC16): Boolean;
    // read a 32-bit header from the bitstream

    function Bitrate: Cardinal;

    function CalculateFrameSize: Cardinal;

    // Scrolling stuff
    function MaxNumberOfFrames(Stream: TBitStream): Integer;
    function MinNumberOfFrames(Stream: TBitStream): Integer;

    function MSPerFrame: Single;  // milliseconds per frame, for time display
    function TotalMS(Stream: TBitStream): Single;
  end;

implementation
uses
  SysUtils;

{ THeader }

const
  mp3_bit_rate_index_tab: array[0..14] of word = (
    0, 32, 40, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256, 320
  );
  mp3_sampling_frequency_tab: array[TSampleFrequency] of word = (
    44100, 48000, 32000, 0
  );

function THeader.Bitrate: Cardinal;
begin
  Result := mp3_bit_rate_index_tab[FBitrateIndex] * 1000;
end;

// calculates framesize in bytes excluding header size
function THeader.CalculateFrameSize: Cardinal;
var
  Val1, Val2: Cardinal;
begin
  Assert(FLayer = 3, 'not layer 3');
  Assert(FVersion = MPEG1, 'invalid format version');

  FFrameSize := 144 * mp3_bit_rate_index_tab[FBitrateIndex] * 1000 div mp3_sampling_frequency_tab[FSampleFrequency];
  if (FPaddingBit <> 0) then
    FFrameSize += 1;

  // Layer III slots
  if (FMode = SingleChannel) then
    Val1 := 17
  else
    Val1 := 32;

  if (FProtectionBit <> 0) then
    Val2 := 0
  else
    Val2 := 2;

  FNumSlots := FFramesize - Val1 - Val2 - 4;
  dec(FFrameSize, 4);  // subtract header size

  Result := FFrameSize;
end;

constructor THeader.Create;
begin
  FFrameSize := 0;
  FNumSlots := 0;
  FCRC := nil;
  FOffset := nil;
  FInitialSync := False;
end;

destructor THeader.Destroy;
begin
  if (FOffset <> nil) then
    FreeMem(FOffset);

  inherited;
end;

function THeader.GetChecksumOK: Boolean;
begin
  Result := (FChecksum = FCRC.Checksum);
end;

function THeader.GetChecksums: Boolean;
begin
  Result := (FProtectionBit = 0);
end;

function THeader.GetFrequency: Cardinal;
begin
  Result := mp3_sampling_frequency_tab[FSampleFrequency];
end;

function THeader.GetPadding: Boolean;
begin
  Result := (FPaddingBit <> 0);
end;

// Returns the maximum number of frames in the stream
function THeader.MaxNumberOfFrames(Stream: TBitStream): Integer;
begin
  Result := Stream.FileSize div (FFrameSize + 4 - FPaddingBit);
end;

// Returns the minimum number of frames in the stream
function THeader.MinNumberOfFrames(Stream: TBitStream): Integer;
begin
  Result := Stream.FileSize div (FFrameSize + 5 - FPaddingBit);
end;

const
  MSPerFrameArray: array[TSampleFrequency] of Single = (26.12245, 24.0, 36.0, 0);
    
function THeader.MSPerFrame: Single;
begin
  Result := MSperFrameArray[FSampleFrequency];
end;

function THeader.ReadHeader(Stream: TBitStream; var CRC: TCRC16): Boolean;
var
  HeaderString, ChannelBitrate: integer;
begin
  Result := False;
  if (not FInitialSync) then begin
    if (not Stream.GetHeader(@HeaderString, INITIAL_SYNC)) then
      exit;

    FVersion := TVersion((HeaderString shr 19) and 1);
    //allow mpeg layer 3 only
    if FVersion <> MPEG1 then
        exit;


    FSampleFrequency := TSampleFrequency((HeaderString shr 10) and 3);
    if (FSampleFrequency = Unknown) then begin
      // report error - not supported header
      exit;
    end;

    Stream.SetSyncWord(HeaderString and $FFF80CC0);

    FInitialSync := True;
  end else begin
    if (not Stream.GetHeader(@HeaderString, STRICT_SYNC)) then begin
      exit;
    end;
  end;

  FLayer := 4 - (HeaderString shr 17) and 3;
  //allow mpeg layer 3 only
  if Flayer <> 3 then
      exit;

  FProtectionBit := (HeaderString shr 16) and 1;
  FBitrateIndex := (HeaderString shr 12) and $F;
  FPaddingBit := (HeaderString shr 9) and 1;
  FMode := TMode((HeaderString shr 6) and 3);
  FModeExtension := (HeaderString shr 4) and 3;

  if (FMode = JointStereo) then
    FIntensityStereoBound := (FModeExtension shl 2) + 4
  else
    FIntensityStereoBound := 0;  // should never be used

  FCopyright := ((HeaderString shr 3) and 1 <> 0);
  FOriginal := ((HeaderString shr 2) and 1 <> 0);

  // calculate number of subbands:
  if (FLayer = 1) then
    FNumberOfSubbands := 32
  else begin
    ChannelBitrate := FBitrateIndex;

    // calculate bitrate per channel:
    if (FMode <> SingleChannel) then
      if (ChannelBitrate <= 4) then
        ChannelBitrate := 1
      else
        dec(ChannelBitrate, 4);

    if ((ChannelBitrate = 1) or (ChannelBitrate = 2)) then begin
      if (FSampleFrequency = SampleFreq_32) then
        FNumberOfSubbands := 12
      else
        FNumberOfSubbands := 8;
    end else begin
      if ((FSampleFrequency = SampleFreq_48) or ((ChannelBitrate >= 3) and (ChannelBitrate <= 5))) then
        FNumberOfSubbands := 27
      else
        FNumberOfSubbands := 30;
    end;
  end;

  if (FIntensityStereoBound > FNumberOfSubbands) then
    FIntensityStereoBound := FNumberOfSubbands;

  // calculate framesize and nSlots
  CalculateFrameSize;

  // read framedata:
  if (not Stream.ReadFrame(FFrameSize)) then begin
    exit;
  end;

  if (FProtectionBit = 0) then begin
    // frame contains a crc checksum
    FChecksum := Stream.GetBits(16);
    if (FCRC = nil) then
      FCRC := TCRC16.Create;

    FCRC.AddBits(HeaderString, 16);
    CRC := FCRC;
  end else
    CRC := nil;

  Result := True;
end;

function THeader.TotalMS(Stream: TBitStream): Single;
begin
  Result := MaxNumberOfFrames(Stream) * MSPerFrame;
end;

end.
