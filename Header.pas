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
  BitStream, L3Tables;

type
  TVersion = (MPEG2_LSF, MPEG1);
  TMode    = (Stereo, JointStereo, DualChannel, SingleChannel);

  TCRC16 = object
  private
    FCRC: Word;

  public
    procedure Init;
    procedure AddBits(BitString: Cardinal; Length: Cardinal);
    function Checksum: Word;
  end;

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
    FInitialSync: Boolean;
    //FCRC: TCRC16;
    FChecksum: Cardinal;
    FFrameSize: Cardinal;
    FNumSlots: Cardinal;

    function GetFrequency: Cardinal;
    function GetChecksums: Boolean;
    function GetPadding: Boolean;

  public
    property Version: TVersion read FVersion;
    property Layer: Cardinal read FLayer;
    property BitrateIndex: Cardinal read FBitrateIndex;
    property SampleFrequency: TSampleFrequency read FSampleFrequency;
    property Frequency: Cardinal read GetFrequency;
    property Mode: TMode read FMode;
    property Checksums: Boolean read GetChecksums;
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

    function ReadHeader(Stream: TBitStream): Boolean;
    // read a 32-bit header from the bitstream

    function Bitrate: Cardinal;

    function CalculateFrameSize: Cardinal;
  end;

implementation

const
  POLYNOMIAL: Word = $8005;
  mp3_bit_rate_index_tab: array[0..14] of word = (
    0, 32, 40, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256, 320
  );
  mp3_sampling_frequency_tab: array[TSampleFrequency] of word = (
    44100, 48000, 32000, 0
  );

{ THeader }

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
  FInitialSync := False;
  //FCRC := FCRC.Create;
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

function THeader.ReadHeader(Stream: TBitStream): Boolean;
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
      FChecksum := Stream.GetBits(16);
      { not useful until whole frame data is checked
      FCRC.Init();
      FCRC.AddBits(HeaderString, 16);  //16bits of header starting from bitrate index
      }
  end;

  Result := True;
end;


{ TCRC16 }

// feed a bitstring to the crc calculation (0 < length <= 32)
procedure TCRC16.AddBits(BitString, Length: Cardinal);
var BitMask: Cardinal;
begin
  BitMask := 1 shl (Length - 1);
  repeat
    if ((FCRC and $8000 = 0) xor (BitString and BitMask = 0)) then begin
      FCRC := FCRC shl 1;
      FCRC := FCRC xor POLYNOMIAL;
    end else
      FCRC := FCRC shl 1;

    BitMask := BitMask shr 1;
  until (BitMask = 0);
end;

function TCRC16.Checksum: Word;
begin
  Result := FCRC;
end;

procedure TCRC16.Init;
begin
  FCRC := $FFFF;
end;

end.
