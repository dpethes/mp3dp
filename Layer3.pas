(*
 *  File:     $RCSfile: Layer3.pas,v $
 *  Revision: $Revision: 1.1.1.1 $
 *  Version : $Id: Layer3.pas,v 1.1.1.1 2002/04/21 12:57:21 fobmagog Exp $
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
unit Layer3;
{$mode delphi}

interface
uses
  classes,
  L3Tables, Shared, BitReserve, BitStream, Header, SynthFilter, Huffman, InvMDT;

const
  // Size of the table of whole numbers raised to 4/3 power.
  // This may be adjusted for performance without any problems.
  POW_TABLE_LIMIT = 512;

type
  PSArray = ^TSArray;
  TSArray = array[0..SBLIMIT-1, 0..SSLIMIT-1] of Single;

  PGrInfo = ^TGrInfo;
  TGrInfo = record
    part2_3_length: word;
    big_values: word;
    global_gain: byte;
    scalefac_compress: byte;
    block_type: byte;
    mixed_block_flag: byte;  //switch_point
    table_select: array[0..2] of byte;
    subblock_gain: array[0..2] of byte;
    region0_count: byte;  //region_address1
    region1_count: byte;
    preflag: byte;
    scalefac_scale: byte;      //0 or 1
    count1table_select: byte;
    window_switching_flag: byte;  //blocksplit flag
  end;

  TSideInfo = record
    main_data_begin: word;
    private_bits: byte;            //unused
    ch: array[0..1] of record
      scfsi: array[0..3] of byte;  //scalefactor selection information
      gr: array[0..1] of TGrInfo;
    end;
  end;

  TScaleFactorsChannel = record
      long: array[0..22] of byte;
      short: array[0..2, 0..12] of byte;  // [window][cb]
  end;

  TScaleFactors = array[0..1] of TScaleFactorsChannel;  //channel 0/1

  { TLayerIII_Decoder }

  TLayerIII_Decoder = class
  private
    FInputSamples: array[0..GRANULE_SAMPLES-1] of Int16;  //huff decoded samples
    FRO: array[0..1] of TSArray;                          //dequantized samples
    FLR: array[0..1] of TSArray;                          //left/right samples
    FOut_1D: array[0..GRANULE_SAMPLES-1] of Single;       //reordered samples of current channel
    FPrevBlock: array[0..1, 0..GRANULE_SAMPLES-1] of Single;
    FNonZero: array[0..1] of UInt16;

    FStream: TBitStream;
    FHeader: THeader;
    FFilter: array[0..1] of TSynthesisFilter;  //separate filter for each channel
    FBuffer: TStream;
    FWhichChannels: TChannels;
    FBR: TBitReserve;
    FSideInfo: TSideInfo;
    FScaleFactors: TScaleFactors;

    FMaxGr: Cardinal;
    FPart2Start: Cardinal;
    FChannels: Cardinal;
    FFirstChannel: Cardinal;
    FLastChannel: Cardinal;
    FSFreq: Cardinal;

    _skipped_frames: integer;
    FGrInfoError: boolean;
    FTryDecodeBrokenFrames: boolean;  //might fail; useful for testing streams with errors

    procedure DecodeGranule(gr: integer);
    procedure FrequencyInversion();
    procedure GetSideInfo;
    procedure GetScaleFactors(ch: Cardinal; gr: Cardinal);
    procedure HuffmanDecode(ch: Cardinal; gr: Cardinal);
    procedure DequantizeSample(var xr: TSArray; ch: Cardinal; gr: Cardinal);
    procedure Reorder(xr: PSArray; ch: Cardinal; gr: Cardinal);
    procedure Stereo();
    procedure Antialias(ch: Cardinal; gr: Cardinal);
    procedure Hybrid(ch: Cardinal; gr: Cardinal);
    procedure DoDownmix;

  public
    constructor Create(Stream: TBitStream; Header: THeader; Buffer: TStream);
    destructor Destroy; override;

    // Decode one frame, filling the buffer with the output samples
    procedure DecodeSingleFrame;
  end;

implementation

{ TLayerIII_Decoder }

// 31 alias-reduction operations between each pair of sub-bands
// with 8 butterflies between each pair
procedure TLayerIII_Decoder.Antialias(ch: Cardinal; gr: Cardinal);
const
  cs: array[0..7] of Single = (
    0.857492925712, 0.881741997318, 0.949628649103, 0.983314592492,
    0.995517816065, 0.999160558175, 0.999899195243, 0.999993155067);

  ca: array[0..7] of Single = (
    -0.5144957554270, -0.4717319685650, -0.3133774542040, -0.1819131996110,
    -0.0945741925262, -0.0409655828852, -0.0141985685725, -0.00369997467375);
var
  gr_info: PGRInfo;
  ss, sb18, sb18lim: integer;
  bu, bd: Single;
  src_idx1, src_idx2: Integer;
begin
  sb18lim := 558;
  gr_info := @FSideInfo.ch[ch].gr[gr];
  if (gr_info.block_type = SHORT_BLOCK) then begin
      if (gr_info.mixed_block_flag = 0) then
          exit
      else
          sb18lim := 18;
  end;

  sb18 := 0;
  while (sb18 < sb18lim) do begin
    for ss := 0 to 7 do begin
      src_idx1 := sb18 + 17 - ss;
      src_idx2 := sb18 + 18 + ss;
      bu := FOut_1D[src_idx1];
      bd := FOut_1D[src_idx2];
      FOut_1D[src_idx1] := (bu * cs[ss]) - (bd * ca[ss]);
      FOut_1D[src_idx2] := (bd * cs[ss]) + (bu * ca[ss]);
    end;
    
    inc(sb18, 18);
  end;
end;

constructor TLayerIII_Decoder.Create(Stream: TBitStream; Header: THeader; Buffer: TStream);
begin
  Assert(Header.Version = MPEG1, 'invalid format version');

  FFilter[0] := TSynthesisFilter.Create();
  FFilter[1] := TSynthesisFilter.Create();

  FStream := Stream;
  FHeader := Header;
  FBuffer := Buffer;
  FWhichChannels := Both;

  if (FHeader.Mode = SingleChannel) then
    FChannels := 1
  else
    FChannels := 2;

  if (FHeader.Version = MPEG1) then
    FMaxGr := 2
  else
    FMaxGr := 1;

  FSFreq := Cardinal(FHeader.SampleFrequency);

  if (FChannels = 2) then begin
    case FWhichChannels of
      Left,
      Downmix: begin
        FFirstChannel := 0;
        FLastChannel := 0;
      end;

      Right: begin
        FFirstChannel := 1;
        FLastChannel := 1;
      end;

      Both: begin
        FFirstChannel := 0;
        FLastChannel := 1;
      end;

      else begin
        FFirstChannel := 0;
        FLastChannel := 1;
      end;
    end;
  end else begin
    FFirstChannel := 0;
    FLastChannel := 0;
  end;

  FillChar(FPrevBlock, Sizeof(FPrevBlock), 0);
  FNonZero[0] := 576;
  FNonZero[1] := 576;

  FBR := TBitReserve.Create;
  _skipped_frames := 0;
  FTryDecodeBrokenFrames := false;
end;

procedure TLayerIII_Decoder.DecodeSingleFrame;
var
  has_error: Boolean;
begin
  Assert(FHeader.Version = MPEG1, 'invalid format version');

  GetSideInfo;
  FBR.NewFrame(FSideInfo.main_data_begin);
  FBR.InsertMainData(FStream.CurrentDataPointer, FHeader.Slots);

  //most likely a SideInfo error: do not process this frame but keep main_data bytes, next frame might need it
  has_error := FBR.InvalidDataBegin or FGrInfoError;
  if not has_error or FTryDecodeBrokenFrames then begin
      DecodeGranule(0);
      DecodeGranule(1);
  end else
      _skipped_frames += 1;

  FBR.EndFrame();
end;

procedure TLayerIII_Decoder.DecodeGranule(gr: integer);
var
  ch, ss, sb, sb18: Cardinal;
  pcm_buffer_pos: PInt16;
  pcm_buffer: array[0..GRANULE_SAMPLES * 2 - 1] of int16;
  i: Integer;
begin
  for ch := 0 to FChannels-1 do begin
      FPart2Start := FBR.bitPosition;

      GetScaleFactors(ch, gr);
      HuffmanDecode(ch, gr);
      DequantizeSample(FRO[ch], ch, gr);
  end;

  Stereo();

  if ((FWhichChannels = Downmix) and (FChannels > 1)) then
      DoDownmix;

  for ch := FFirstChannel to FLastChannel do begin
      Reorder(@FLR[ch], ch, gr);
      Antialias(ch, gr);
      Hybrid(ch, gr);
      FrequencyInversion();

      // Polyphase synthesis
      pcm_buffer_pos := @pcm_buffer[ch];
      for ss := 0 to SSLIMIT-1 do begin
          sb := 0;
          sb18 := 0;
          while (sb18 < 576) do begin
            FFilter[ch].InputSample(FOut_1D[sb18 + ss], sb);
            inc(sb18, 18);
            inc(sb);
          end;

          FFilter[ch].CalculatePCMSamples(pcm_buffer_pos);
          pcm_buffer_pos += 64;
      end;
  end;

  //copy first channel samples into second channel for stereo output of mono stream
  if FChannels = 1 then begin
      for i := 0 to GRANULE_SAMPLES - 1 do
          pcm_buffer[i * 2 + 1] := pcm_buffer[i * 2];
  end;

  //both channels are now decoded and interleaved in pcm_buffer
  FBuffer.WriteBuffer(pcm_buffer, GRANULE_SAMPLES * 2 {channels} * 2 {sample size});
end;

{ In order to compensate for frequency inversions in the synthesis polyphase filterbank
  every odd time sample of every odd subband is multiplied with -1 ()
}
procedure TLayerIII_Decoder.FrequencyInversion();
var
  sb18, ss: integer;
begin
  sb18 := 18;
  while (sb18 < 576) do begin
    ss := 1;
    while (ss < SSLIMIT) do begin
      FOut_1D[sb18 + ss] := -FOut_1D[sb18 + ss];
      inc(ss, 2);
    end;

    inc(sb18, 36);
  end;
end;

procedure TLayerIII_Decoder.DequantizeSample(var xr: TSArray; ch: Cardinal; gr: Cardinal);
var gr_info: PGRInfo;
    cb: Integer;
    j, next_cb_boundary, cb_begin, cb_width: Integer;
    index, t_index: integer;
    g_gain: Single;
    xr1d: PSingleArray;
    idx: Cardinal;
    abv: integer;
begin
  gr_info := @FSideInfo.ch[ch].gr[gr];
  cb := 0;
  index := 0;
  cb_begin := 0;
  cb_width := 0;
  xr1d := @xr[0, 0];
  FillByte(xr1d^, sizeof(TSArray), 0);

  // choose correct scalefactor band per block type, initalize boundary
  if (gr_info.block_type = SHORT_BLOCK) then begin
    if (gr_info.mixed_block_flag <> 0) then
      next_cb_boundary := sfBandIndex[FSFreq].l[1]  // LONG blocks: 0,1,3
    else begin
      cb_width := sfBandIndex[FSFreq].s[1];
      next_cb_boundary := (cb_width shl 2) - cb_width;
      cb_begin := 0;
    end;
  end else
    next_cb_boundary := sfBandIndex[FSFreq].l[1];  // LONG blocks: 0,1,3

  // scale by global gain first
  g_gain := L3_global_gain_table[gr_info.global_gain];  //Power(2.0 , (0.25 * (gain - 210.0)));

  for j := 0 to FNonZero[ch]-1 do begin
      abv := FInputSamples[j];
      if (abv >= 0) then
        xr1d[j] := g_gain * L3_pow_43[abv]
      else
        xr1d[j] := -g_gain * L3_pow_43[-abv];
  end;

  // apply formula per block type
  for j := 0 to FNonZero[ch]-1 do begin
    if (index = next_cb_boundary) then begin  // Adjust critical band boundary
      if (gr_info.block_type = SHORT_BLOCK) then begin
        if (gr_info.mixed_block_flag <> 0) then begin
          if (index = sfBandIndex[FSFreq].l[8]) then begin
            next_cb_boundary := sfBandIndex[FSFreq].s[4];
            next_cb_boundary := (next_cb_boundary shl 2) - next_cb_boundary;
            cb := 3;
            cb_width := sfBandIndex[FSFreq].s[4] - sfBandIndex[FSFreq].s[3];
            cb_begin := sfBandIndex[FSFreq].s[3];
            cb_begin := (cb_begin shl 2) - cb_begin;
          end else if (index < sfBandIndex[FSFreq].l[8]) then begin
            inc(cb);
            next_cb_boundary := sfBandIndex[FSFreq].l[cb+1];
          end else begin
            inc(cb);
            next_cb_boundary := sfBandIndex[FSFreq].s[cb+1];
            next_cb_boundary := (next_cb_boundary shl 2) - next_cb_boundary;
            cb_begin := sfBandIndex[FSFreq].s[cb];
            cb_width := sfBandIndex[FSFreq].s[cb+1] - cb_begin;
            cb_begin := (cb_begin shl 2) - cb_begin;
          end;
        end else begin
          inc(cb);
          next_cb_boundary := sfBandIndex[FSFreq].s[cb+1];
          next_cb_boundary := (next_cb_boundary shl 2) - next_cb_boundary;
          cb_begin := sfBandIndex[FSFreq].s[cb];
          cb_width := sfBandIndex[FSFreq].s[cb+1] - cb_begin;
          cb_begin := (cb_begin shl 2) - cb_begin;
        end;
      end else begin  // long blocks
        inc(cb);
        next_cb_boundary := sfBandIndex[FSFreq].l[cb+1];
      end;
    end;

    // Do long/short dependent scaling operations
    if (gr_info.block_type = SHORT_BLOCK)
      and ((gr_info.mixed_block_flag = 0) or ((gr_info.mixed_block_flag <> 0) and (j >= 36))) then begin
        // xr[sb][ss] *= pow(2.0, ((-2.0 * gr_info->subblock_gain[t_index]) -(0.5 * (1.0 + gr_info->scalefac_scale) * scalefac[ch].s[t_index][cb])));
        t_index := (index - cb_begin) div cb_width;
        idx := FScaleFactors[ch].short[t_index][cb] shl gr_info.scalefac_scale;
        idx := idx + (gr_info.subblock_gain[t_index] shl 2);
    end else begin
        // LONG block types 0,1,3 & 1st 2 subbands of switched blocks
        // xr[sb][ss] *= pow(2.0, -0.5 * (1.0+gr_info->scalefac_scale) * (scalefac[ch].l[cb] + gr_info->preflag * pretab[cb]));
        idx := FScaleFactors[ch].long[cb];
        if (gr_info.preflag <> 0) then
            idx := idx + pretab[cb];

        idx := idx shl gr_info.scalefac_scale;
    end;
    xr1d[j] := xr1d[j] * L3_two_to_negative_half_pow[idx];
    inc(index);
  end;
end;

destructor TLayerIII_Decoder.Destroy;
begin
  FBR.Free;
  FFilter[0].Free;
  FFilter[1].Free;

  inherited Destroy;
end;

procedure TLayerIII_Decoder.DoDownmix;
var ss, sb: Cardinal;
begin
  for sb := 0 to SBLIMIT-1 do begin
    ss := 0;
    while (ss < SSLIMIT) do begin
      FLR[0][sb][ss]   := (FLR[0][sb][ss]   + FLR[1][sb][ss])   * 0.5;
      FLR[0][sb][ss+1] := (FLR[0][sb][ss+1] + FLR[1][sb][ss+1]) * 0.5;
      FLR[0][sb][ss+2] := (FLR[0][sb][ss+2] + FLR[1][sb][ss+2]) * 0.5;
      inc(ss, 3);
    end;
  end;
end;

{$OPTIMIZATION NOLOOPUNROLL}
procedure TLayerIII_Decoder.GetScaleFactors(ch: Cardinal; gr: Cardinal);

  function getBits(n: integer): byte;
  begin
    if n = 0 then
        result := 0
    else
        result := FBR.hgetbits(n);
  end;

var sfb, window: Integer;
    gr_info: PGRInfo;
    length0, length1: Integer;
begin
  gr_info := @FSideInfo.ch[ch].gr[gr];
  length0 := slen[0, gr_info.scalefac_compress];  //length0/1 can be 0 bits, skip reading ScaleFactors in that case
  length1 := slen[1, gr_info.scalefac_compress];

  //these are always zero for layer 3 (cb_limit = 21, cb_limit_short = 12)
  with FScaleFactors[ch] do begin
      long[21] := 0;
      long[22] := 0;
      short[0, 12] := 0;
      short[1, 12] := 0;
      short[2, 12] := 0;
  end;

  if (gr_info.block_type = SHORT_BLOCK) then begin
      with FScaleFactors[ch] do begin
          if (gr_info.mixed_block_flag <> 0) then begin  // MIXED
              for sfb := 0 to 7 do
                  long[sfb] := getBits(length0);
              for sfb := 3 to 5 do
                for window := 0 to 2 do
                  short[window, sfb] := getBits(length0);
          end else begin  // SHORT
              for sfb := 0 to 5 do
                for window := 0 to 2 do
                  short[window, sfb] := getBits(length0);
          end;
          for sfb := 6 to 11 do
            for window := 0 to 2 do
              short[window, sfb] := getBits(length1);
      end;
  end else begin  // LONG types 0,1,3
      with FScaleFactors[ch] do begin
          if ((FSideInfo.ch[ch].scfsi[0] = 0) or (gr = 0)) then begin
              for sfb := 0 to 5 do
                  long[sfb] := getBits(length0);
          end;

          if ((FSideInfo.ch[ch].scfsi[1] = 0) or (gr = 0)) then begin
              for sfb := 6 to 10 do
                  long[sfb] := getBits(length0);
          end;

          if ((FSideInfo.ch[ch].scfsi[2] = 0) or (gr = 0)) then begin
              for sfb := 11 to 15 do
                  long[sfb] := getBits(length1);
          end;

          if ((FSideInfo.ch[ch].scfsi[3] = 0) or (gr = 0)) then begin
              for sfb := 16 to 20 do
                  long[sfb] := getBits(length1);
          end;
      end;
  end;
end;
{$OPTIMIZATION DEFAULT}

// Reads the side info from the stream, assuming the entire
// frame has been read already.

// Mono   : 136 bits (= 17 bytes)
// Stereo : 256 bits (= 32 bytes)
procedure TLayerIII_Decoder.GetSideInfo;
var
  ch, gr: Cardinal;
  gr_info: TGrInfo;
begin
  Assert(FHeader.Version = MPEG1, 'invalid format version');

  FGrInfoError := false;
  FSideInfo.main_data_begin := FStream.GetBits(9);
  if (FChannels = 1) then
    FSideInfo.private_bits := FStream.GetBits(5)
  else
    FSideInfo.private_bits := FStream.GetBits(3);

  for ch := 0 to FChannels-1 do begin
      with FSideInfo.ch[ch] do begin
          scfsi[0] := FStream.GetBits(1);
          scfsi[1] := FStream.GetBits(1);
          scfsi[2] := FStream.GetBits(1);
          scfsi[3] := FStream.GetBits(1);
      end;
  end;

  for gr := 0 to 1 do begin
      for ch := 0 to FChannels-1 do begin
          gr_info.part2_3_length := FStream.GetBits(12);
          gr_info.big_values := FStream.GetBits(9);
          if gr_info.big_values * 2 > GRANULE_SAMPLES then
              FGrInfoError := true;
          gr_info.global_gain := FStream.GetBits(8);
          gr_info.scalefac_compress := FStream.GetBits(4);
          gr_info.window_switching_flag := FStream.GetBits(1);
          if gr_info.window_switching_flag <> 0 then begin
              gr_info.block_type := FStream.GetBits(2);
              if gr_info.block_type = 0 then  // reserved
                  FGrInfoError := true;
              gr_info.mixed_block_flag := FStream.GetBits(1);

              gr_info.table_select[0] := FStream.GetBits(5);  //region 0,1 only; region 2 unused in short block
              gr_info.table_select[1] := FStream.GetBits(5);

              gr_info.subblock_gain[0] := FStream.GetBits(3);  //window 0..2
              gr_info.subblock_gain[1] := FStream.GetBits(3);
              gr_info.subblock_gain[2] := FStream.GetBits(3);

              // Set region_count parameters since they are implicit in this case.
              gr_info.region0_count := 7;
              if (gr_info.block_type = SHORT_BLOCK) and (gr_info.mixed_block_flag = 0) then
                  gr_info.region0_count := 8;

              gr_info.region1_count := 20 - gr_info.region0_count;
          end else begin
              gr_info.block_type := 0;
              gr_info.mixed_block_flag := 0;
              gr_info.table_select[0] := FStream.GetBits(5);
              gr_info.table_select[1] := FStream.GetBits(5);
              gr_info.table_select[2] := FStream.GetBits(5);
              gr_info.region0_count := FStream.GetBits(4);
              gr_info.region1_count := FStream.GetBits(3);
          end;
          gr_info.preflag := FStream.GetBits(1);
          gr_info.scalefac_scale := FStream.GetBits(1);
          gr_info.count1table_select := FStream.GetBits(1);

          FSideInfo.ch[ch].gr[gr] := gr_info;
      end;
  end;
end;

procedure TLayerIII_Decoder.HuffmanDecode(ch: Cardinal; gr: Cardinal);
var i: Cardinal;
    x, y, v, w: Integer;
    part2_3_end: Integer;
    num_bits: Integer;
    region1Start: Cardinal;
    region2Start: Cardinal;
    index, bits_to_skip: Integer;
    big_values: uint32;
    check_big_values_bitcount: boolean;
    h: PHuffCodeTab;
    gr_info: TGrInfo;
begin
  FillByte(FInputSamples, sizeof(FInputSamples), 0);
  gr_info := FSideInfo.ch[ch].gr[gr];

  part2_3_end := FPart2Start + gr_info.part2_3_length;
  check_big_values_bitcount := false;

  // Find region boundaries
  if (gr_info.block_type = SHORT_BLOCK) then begin
    region1Start := 36;   // sfb[9/3]*3=36
    region2Start := 576;  // No Region2 for short block case
  end else begin
    region1Start := sfBandIndex[FSFreq].l[gr_info.region0_count + 1];
    region2Start := sfBandIndex[FSFreq].l[gr_info.region0_count + gr_info.region1_count + 2];
  end;

  index := 0;
  // Read bigvalues area
  i := 0;
  big_values := gr_info.big_values * 2;
  if big_values > GRANULE_SAMPLES then begin  //check against corrupted streams
      big_values := GRANULE_SAMPLES;
      check_big_values_bitcount := true;
  end;
  while i < big_values do begin
    if (i < region1Start) then
      h := @g_hufftables[gr_info.table_select[0]]
    else if (i < region2Start) then
      h := @g_hufftables[gr_info.table_select[1]]
    else
      h := @g_hufftables[gr_info.table_select[2]];

    HuffmanDecoder(h, x, y, v, w, FBR);

    FInputSamples[index] := x;
    FInputSamples[index+1] := y;

    inc(index, 2);
    inc(i, 2);

    if check_big_values_bitcount then begin
        num_bits := FBR.bitPosition;
        //seems we read way too far already
        if (num_bits > part2_3_end) then begin
            bits_to_skip := num_bits - part2_3_end;
            FBR.RewindBits(bits_to_skip);
            dec(index, 2);
            num_bits -= bits_to_skip;
            break;
        end;
    end;
  end;

  // Read count1 area
  h := @g_hufftables[gr_info.count1table_select + 32];
  num_bits := FBR.bitPosition;

  while ((num_bits < part2_3_end) and (index + 4 < GRANULE_SAMPLES)) do begin
    HuffmanDecoder(h, x, y, v, w, FBR);

    FInputSamples[index] := v;
    FInputSamples[index+1] := w;
    FInputSamples[index+2] := x;
    FInputSamples[index+3] := y;

    inc(index, 4);
    num_bits := FBR.bitPosition;
  end;

  //seems we read way too far - this happens quite often, wrong part2_3_length being written by encoder? Some decoder error?
  if (num_bits > part2_3_end) then begin
      bits_to_skip := num_bits - part2_3_end;
      FBR.RewindBits(bits_to_skip);
      dec(index, 4);
      num_bits -= bits_to_skip;
  end;

  // Dismiss stuffing bits (test with si.bit)
  if (num_bits < part2_3_end) then begin
      bits_to_skip := part2_3_end - num_bits;
      FBR.SkipBits(bits_to_skip);
  end;

  // Zero out rest
  if (index < 576) then
    FNonZero[ch] := index
  else
    FNonZero[ch] := 576;
end;

procedure TLayerIII_Decoder.Hybrid(ch: Cardinal; gr: Cardinal);
var rawout: array[0..35] of Single;
    block_type: integer;
    gr_info: PGRInfo;
    tsOut: PSingleArray;
    prvblk: PSingleArray;
    sb18: Cardinal;
begin
  gr_info := @FSideInfo.ch[ch].gr[gr];

  sb18 := 0;
  while (sb18 < 576) do begin
    if (gr_info.block_type = SHORT_BLOCK) and (gr_info.mixed_block_flag <> 0) and (sb18 < 36) then
      block_type := 0
    else
      block_type := gr_info.block_type;

    tsOut := @FOut_1D[sb18];
    InvMDCT(tsOut, @rawout, block_type);

    // overlap addition
    prvblk := @FPrevblock[ch, sb18];

    tsOut[0]   := rawout[0]  + prvblk[0];
    prvblk[0]  := rawout[18];
    tsOut[1]   := rawout[1]  + prvblk[1];
    prvblk[1]  := rawout[19];
    tsOut[2]   := rawout[2]  + prvblk[2];
    prvblk[2]  := rawout[20];
    tsOut[3]   := rawout[3]  + prvblk[3];
    prvblk[3]  := rawout[21];
    tsOut[4]   := rawout[4]  + prvblk[4];
    prvblk[4]  := rawout[22];
    tsOut[5]   := rawout[5]  + prvblk[5];
    prvblk[5]  := rawout[23];
    tsOut[6]   := rawout[6]  + prvblk[6];
    prvblk[6]  := rawout[24];
    tsOut[7]   := rawout[7]  + prvblk[7];
    prvblk[7]  := rawout[25];
    tsOut[8]   := rawout[8]  + prvblk[8];
    prvblk[8]  := rawout[26];
    tsOut[9]   := rawout[9]  + prvblk[9];
    prvblk[9]  := rawout[27];
    tsOut[10]  := rawout[10] + prvblk[10];
    prvblk[10] := rawout[28];
    tsOut[11]  := rawout[11] + prvblk[11];
    prvblk[11] := rawout[29];
    tsOut[12]  := rawout[12] + prvblk[12];
    prvblk[12] := rawout[30];
    tsOut[13]  := rawout[13] + prvblk[13];
    prvblk[13] := rawout[31];
    tsOut[14]  := rawout[14] + prvblk[14];
    prvblk[14] := rawout[32];
    tsOut[15]  := rawout[15] + prvblk[15];
    prvblk[15] := rawout[33];
    tsOut[16]  := rawout[16] + prvblk[16];
    prvblk[16] := rawout[34];
    tsOut[17]  := rawout[17] + prvblk[17];
    prvblk[17] := rawout[35];
    
    inc(sb18, 18);
  end;
end;

procedure TLayerIII_Decoder.Reorder(xr: PSArray; ch: Cardinal; gr: Cardinal);
var gr_info: PGRInfo;
    freq, freq3: Cardinal;
    sfb, sfb_start, sfb_start3, sfb_lines: Cardinal;
    src_line, des_line: Integer;
    xr1d: PSingleArray;
    index: Cardinal;
begin
  xr1d := @xr[0, 0];
  gr_info := @FSideInfo.ch[ch].gr[gr];
  if (gr_info.block_type = SHORT_BLOCK) then begin
    FillByte(FOut_1D, sizeof(FOut_1D), 0);
      
    if (gr_info.mixed_block_flag <> 0) then begin
      // NO REORDER FOR LOW 2 SUBBANDS
      for index := 0 to 36-1 do
        FOut_1D[index] := xr1d[index];

      // REORDERING FOR REST SWITCHED SHORT
      sfb_start := sfBandIndex[FSFreq].s[3];
      sfb_lines := Cardinal(sfBandIndex[FSFreq].s[4]) - sfb_start;
      for sfb := 3 to 12 do begin
        sfb_start3 := (sfb_start shl 2) - sfb_start;
        freq3 := 0;
        for freq := 0 to sfb_lines-1 do begin
          src_line := sfb_start3 + freq;
          des_line := sfb_start3 + freq3;
          FOut_1D[des_line] := xr1d[src_line];
          inc(src_line, sfb_lines);
          inc(des_line);
          FOut_1D[des_line] := xr1d[src_line];
          inc(src_line, sfb_lines);
          inc(des_line);
          FOut_1D[des_line] := xr1d[src_line];
          inc(freq3, 3);
        end;
        sfb_start := sfBandIndex[FSFreq].s[sfb];
        sfb_lines := Cardinal(sfBandIndex[FSFreq].s[sfb+1]) - sfb_start;
      end;
    end else begin  // pure short
      for index := 0 to 576-1 do
        FOut_1D[index] := xr1d[L3_short_reorder_table[FSFreq][index]];
    end;
  end else begin
      // no reorder for long blocks
      move(xr1d^, FOut_1D, sizeof(FOut_1D));
  end;
end;

procedure TLayerIII_Decoder.Stereo();
var
  sb, ss: Integer;
  mode_ext: Cardinal;
  ms_stereo, i_stereo: Boolean;
begin
  if (FChannels = 1) then begin  // mono , bypass xr[0][][] to lr[0][][]
      Assert(sizeof(FRO[0]) = GRANULE_SAMPLES * 4);
      move(FRO[0], FLR[0], sizeof(FRO[0]));
      exit;
  end;

  mode_ext := FHeader.ModeExtension;
  ms_stereo := (FHeader.Mode = JointStereo) and (Mode_Ext and $2 <> 0);
  i_stereo  := (FHeader.Mode = JointStereo) and (Mode_Ext and $1 <> 0);

  // removed intensity stereo code due to lack of samples; lame doesn't support it
  Assert(i_stereo = false, 'intensity stereo not supported atm');

  if (ms_stereo) then begin
      for sb := 0 to SBLIMIT-1 do
          for ss := 0 to SSLIMIT-1 do begin
              FLR[0][sb][ss] := (FRO[0][sb][ss] + FRO[1][sb][ss]) * 0.707106781;  //1/sqrt(2)
              FLR[1][sb][ss] := (FRO[0][sb][ss] - FRO[1][sb][ss]) * 0.707106781;
          end;
  end else begin
      Assert(sizeof(FRO) = GRANULE_SAMPLES * 4 * 2);
      move(FRO, FLR, sizeof(FRO));
  end;
end;

end.
