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
  Shared, BitReserve, BitStream, Header, SynthFilter;

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
    window_switching_flag: byte;  //blocksplit flag
    block_type: byte;
    mixed_block_flag: byte;  //switch_point
    table_select: array[0..2] of byte;
    subblock_gain: array[0..2] of byte;
    region0_count: byte;  //region_address1
    region1_count: byte;
    preflag: byte;
    scalefac_scale: byte;
    count1table_select: byte;
  end;

  TSideInfo = record
    main_data_begin: word;
    private_bits: byte;            //unused
    ch: array[0..1] of record
      scfsi: array[0..3] of byte;  //scalefactor selection information
      gr: array[0..1] of TGrInfo;
    end;
  end;

  TScaleFactors = array[0..1] of record  //channel 0/1
    long: array[0..22] of byte;
    short: array[0..2, 0..12] of byte;  // [window][cb]
  end;

  { TLayerIII_Decoder }

  TLayerIII_Decoder = class
  private
    FRO: array[0..1] of TSArray;
    FLR: array[0..1] of TSArray;
    FInputSamples:  array[0..GRANULE_SAMPLES-1] of Int16;
    FOut_1D: array[0..GRANULE_SAMPLES-1] of Single;
    FPrevBlock: array[0..1, 0..GRANULE_SAMPLES-1] of Single;
    FNonZero: array[0..1] of UInt16;

    FStream: TBitStream;
    FHeader: THeader;
    FFilter1, FFilter2: TSynthesisFilter;
    FBuffer: TStream;
    FWhichChannels: TChannels;
    FBR: TBitReserve;
    FSideInfo: TSideInfo;
    FScaleFactors: TScaleFactors;

    FMaxGr: Cardinal;
    FFrameStart: Integer;
    FPart2Start: Cardinal;
    FChannels: Cardinal;
    FFirstChannel: Cardinal;
    FLastChannel: Cardinal;
    FSFreq: Cardinal;

    function GetSideInfo: Boolean;
    procedure GetScaleFactors(ch: Cardinal; gr: Cardinal);
    procedure HuffmanDecode(ch: Cardinal; gr: Cardinal);
    procedure DequantizeSample(var xr: TSArray; ch: Cardinal; gr: Cardinal);
    procedure Reorder(xr: PSArray; ch: Cardinal; gr: Cardinal);
    procedure Stereo(gr: Cardinal);
    procedure Antialias(ch: Cardinal; gr: Cardinal);
    procedure Hybrid(ch: Cardinal; gr: Cardinal);
    procedure DoDownmix;

  public
    constructor Create(Stream: TBitStream; Header: THeader; Buffer: TStream);
    destructor Destroy; override;

    // Notify decoder that a seek is being made
    procedure SeekNotify;

    // Decode one frame, filling the buffer with the output samples
    procedure DecodeSingleFrame;
  end;

implementation
uses
  SysUtils, Huffman, Math, InvMDT, L3Tables;

{ TLayerIII_Decoder }

procedure TLayerIII_Decoder.Antialias(ch: Cardinal; gr: Cardinal);
var ss, sb18, sb18lim: Cardinal;
    gr_info: PGRInfo;
    bu, bd: Single;
    src_idx1, src_idx2: Integer;
begin
  gr_info := @FSideInfo.ch[ch].gr[gr];

  // 31 alias-reduction operations between each pair of sub-bands
  // with 8 butterflies between each pair
  if (gr_info.window_switching_flag <> 0) and (gr_info.block_type = 2) and (gr_info.mixed_block_flag = 0) then
    exit;

  if (gr_info.window_switching_flag <> 0) and (gr_info.mixed_block_flag <> 0) and (gr_info.block_type = 2) then
    sb18lim := 18
  else
    sb18lim := 558;

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

  FFilter1 := TSynthesisFilter.Create(0);
  FFilter2 := TSynthesisFilter.Create(1);

  FStream := Stream;
  FHeader := Header;
  FBuffer := Buffer;
  FWhichChannels := Both;

  FFrameStart := 0;
  if (FHeader.Mode = SingleChannel) then
    FChannels := 1
  else
    FChannels := 2;

  if (FHeader.Version = MPEG1) then
    FMaxGr := 2
  else
    FMaxGr := 1;

  FSFreq := Cardinal(FHeader.SampleFrequency);//FHeader.Frequency;
  if (FHeader.Version = MPEG1) then
    FSFreq := FSFreq + 3;

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
end;

procedure TLayerIII_Decoder.DecodeSingleFrame;
var nSlots: Cardinal;
    flush_main, ch, ss, sb, sb18: Cardinal;
    main_data_end: Integer;
    bytes_to_discard: Integer;
    i, gr: Cardinal;
    pcm_buffer: array[0..GRANULE_SAMPLES * 2 - 1] of int16;
    pcm_buffer_pos: PInt16;
begin
  Assert(FHeader.Version = MPEG1, 'invalid format version');

  nSlots := FHeader.Slots;
  GetSideInfo;

  for i := 0 to nSlots-1 do
    FBR.hputbuf(FStream.GetBits(8));

  main_data_end := FBR.hsstell shr 3;  // of previous frame

  flush_main := (FBR.hsstell and 7);
  if (flush_main <> 0) then begin
    FBR.hgetbits(8 - flush_main);
    inc(main_data_end);
  end;

  bytes_to_discard := FFrameStart - main_data_end - FSideInfo.main_data_begin;
  inc(FFrameStart, nSlots);

  if (bytes_to_discard < 0) then
    exit;

  if (main_data_end > 4096) then begin
    dec(FFrameStart, 4096);
    FBR.rewindNbytes(4096);
  end;

  while (bytes_to_discard > 0) do begin
    FBR.hgetbits(8);
    dec(bytes_to_discard);
  end;

  for gr := 0 to FMaxGr-1 do begin
    for ch := 0 to FChannels-1 do begin
      FPart2Start := FBR.hsstell;

      GetScaleFactors(ch, gr);
      HuffmanDecode(ch, gr);
      DequantizeSample(FRO[ch], ch, gr);
    end;

    Stereo(gr);

    if ((FWhichChannels = Downmix) and (FChannels > 1)) then
      DoDownmix;

    for ch := FFirstChannel to FLastChannel do begin
      Reorder(@FLR[ch], ch, gr);
      Antialias(ch, gr);
      Hybrid(ch, gr);

      sb18 := 18;
      while (sb18 < 576) do begin  // Frequency inversion
        ss := 1;
        while (ss < SSLIMIT) do begin
          FOut_1D[sb18 + ss] := -FOut_1D[sb18 + ss];
          inc(ss, 2);
        end;

        inc(sb18, 36);
      end;

      // Polyphase synthesis
      if ((ch = 0) or (FWhichChannels = Right)) then begin
        pcm_buffer_pos := @pcm_buffer[0];
        for ss := 0 to SSLIMIT-1 do begin
          sb := 0;
          sb18 := 0;
          while (sb18 < 576) do begin
            FFilter1.InputSample(FOut_1D[sb18 + ss], sb);
            inc(sb18, 18);
            inc(sb);
          end;

          FFilter1.CalculatePCMSamples(pcm_buffer_pos);
          pcm_buffer_pos += 64;
        end;
      end else begin
        pcm_buffer_pos := @pcm_buffer[1];
        for ss := 0 to SSLIMIT-1 do begin
          sb := 0;
          sb18 := 0;
          while (sb18 < 576) do begin
            FFilter2.InputSample(FOut_1D[sb18 + ss], sb);
            inc(sb18, 18);
            inc(sb);
          end;

          FFilter2.CalculatePCMSamples(pcm_buffer_pos);
          pcm_buffer_pos += 64;
        end;
      end;
    end;

    //both channels are now decoded and interleaved in pcm_buffer
    FBuffer.WriteBuffer(pcm_buffer, GRANULE_SAMPLES * 2 {channels} * 2 {sample size});
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

  // choose correct scalefactor band per block type, initalize boundary
  if (gr_info.window_switching_flag <> 0) and (gr_info.block_type = 2) then begin
    if (gr_info.mixed_block_flag <> 0) then
      next_cb_boundary := sfBandIndex[FSFreq].l[1]  // LONG blocks: 0,1,3
    else begin
      cb_width := sfBandIndex[FSFreq].s[1];
      next_cb_boundary := (cb_width shl 2) - cb_width;
      cb_begin := 0;
    end;
  end else
    next_cb_boundary := sfBandIndex[FSFreq].l[1];  // LONG blocks: 0,1,3

  // Compute overall (global) scaling.
  g_gain := Power(2.0 , (0.25 * (gr_info.global_gain - 210.0)));

  for j := 0 to FNonZero[ch]-1 do begin
    if (FInputSamples[j] = 0) then
      xr1d[j] := 0.0
    else begin
      abv := FInputSamples[j];
      if (abv > 0) then
        xr1d[j] := g_gain * pow_43[abv]
      else
        xr1d[j] := -g_gain * pow_43[-abv];
    end;
  end;

  // apply formula per block type
  for j := 0 to FNonZero[ch]-1 do begin
    if (index = next_cb_boundary) then begin  // Adjust critical band boundary
      if (gr_info.window_switching_flag <> 0) and (gr_info.block_type = 2) then begin
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
    if (gr_info.window_switching_flag <> 0) and (((gr_info.block_type = 2) and (gr_info.mixed_block_flag = 0)) or
       ((gr_info.block_type = 2) and (gr_info.mixed_block_flag <> 0) and (j >= 36))) then begin
      t_index := (index - cb_begin) div cb_width;
(*            xr[sb][ss] *= pow(2.0, ((-2.0 * gr_info->subblock_gain[t_index])
                                    -(0.5 * (1.0 + gr_info->scalefac_scale)
                                      * scalefac[ch].s[t_index][cb]))); *)
      idx := FScaleFactors[ch].short[t_index][cb] shl gr_info.scalefac_scale;
      idx := idx + (gr_info.subblock_gain[t_index] shl 2);
    end else begin  // LONG block types 0,1,3 & 1st 2 subbands of switched blocks
(*				xr[sb][ss] *= pow(2.0, -0.5 * (1.0+gr_info->scalefac_scale)
                                * (scalefac[ch].l[cb]
                                + gr_info->preflag * pretab[cb])); *)
      idx := FScaleFactors[ch].long[cb];
      if (gr_info.preflag <> 0) then
        idx := idx + pretab[cb];

      idx := idx shl gr_info.scalefac_scale;
    end;
    xr1d[j] := xr1d[j] * two_to_negative_half_pow[idx];
    inc(index);
  end;

  for j := FNonZero[ch] to GRANULE_SAMPLES-1 do
    xr1d[j] := 0.0;
end;

destructor TLayerIII_Decoder.Destroy;
begin
  FreeAndNil(FBR);
  FFilter1.Free;
  FFilter2.Free;

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


procedure TLayerIII_Decoder.GetScaleFactors(ch: Cardinal; gr: Cardinal);
var sfb, window: Integer;
    gr_info: PGRInfo;
    scale_comp, length0, length1: Integer;
begin
  gr_info := @FSideInfo.ch[ch].gr[gr];
  scale_comp := gr_info.scalefac_compress;
  length0 := slen[0, scale_comp];
  length1 := slen[1, scale_comp];

  if ((gr_info.window_switching_flag <> 0) and (gr_info.block_type = 2)) then begin 
      if (gr_info.mixed_block_flag <> 0) then begin  // MIXED
          for sfb := 0 to 7 do
            FScaleFactors[ch].long[sfb] := FBR.hgetbits(slen[0, gr_info.scalefac_compress]);
          for sfb := 3 to 5 do
            for window := 0 to 2 do
              FScaleFactors[ch].short[window, sfb] := FBR.hgetbits(slen[0, gr_info.scalefac_compress]);
          for sfb := 6 to 11 do
            for window := 0 to 2 do
              FScaleFactors[ch].short[window, sfb] := FBR.hgetbits(slen[1, gr_info.scalefac_compress]);
          sfb := 12;
          for window := 0 to 2 do
            FScaleFactors[ch].short[window, sfb] := 0;
      end else begin  // SHORT
          with FScaleFactors[ch] do begin
              short[0, 0]  := FBR.hgetbits(length0);
              short[1, 0]  := FBR.hgetbits(length0);
              short[2, 0]  := FBR.hgetbits(length0);
              short[0, 1]  := FBR.hgetbits(length0);
              short[1, 1]  := FBR.hgetbits(length0);
              short[2, 1]  := FBR.hgetbits(length0);
              short[0, 2]  := FBR.hgetbits(length0);
              short[1, 2]  := FBR.hgetbits(length0);
              short[2, 2]  := FBR.hgetbits(length0);
              short[0, 3]  := FBR.hgetbits(length0);
              short[1, 3]  := FBR.hgetbits(length0);
              short[2, 3]  := FBR.hgetbits(length0);
              short[0, 4]  := FBR.hgetbits(length0);
              short[1, 4]  := FBR.hgetbits(length0);
              short[2, 4]  := FBR.hgetbits(length0);
              short[0, 5]  := FBR.hgetbits(length0);
              short[1, 5]  := FBR.hgetbits(length0);
              short[2, 5]  := FBR.hgetbits(length0);
              short[0, 6]  := FBR.hgetbits(length1);
              short[1, 6]  := FBR.hgetbits(length1);
              short[2, 6]  := FBR.hgetbits(length1);
              short[0, 7]  := FBR.hgetbits(length1);
              short[1, 7]  := FBR.hgetbits(length1);
              short[2, 7]  := FBR.hgetbits(length1);
              short[0, 8]  := FBR.hgetbits(length1);
              short[1, 8]  := FBR.hgetbits(length1);
              short[2, 8]  := FBR.hgetbits(length1);
              short[0, 9]  := FBR.hgetbits(length1);
              short[1, 9]  := FBR.hgetbits(length1);
              short[2, 9]  := FBR.hgetbits(length1);
              short[0, 10] := FBR.hgetbits(length1);
              short[1, 10] := FBR.hgetbits(length1);
              short[2, 10] := FBR.hgetbits(length1);
              short[0, 11] := FBR.hgetbits(length1);
              short[1, 11] := FBR.hgetbits(length1);
              short[2, 11] := FBR.hgetbits(length1);
              short[0, 12] := 0;
              short[1, 12] := 0;
              short[2, 12] := 0;
          end;
      end;
  end else begin  // LONG types 0,1,3
      with FScaleFactors[ch] do begin
          if ((FSideInfo.ch[ch].scfsi[0] = 0) or (gr = 0)) then begin
              long[0]  := FBR.hgetbits(length0);
              long[1]  := FBR.hgetbits(length0);
              long[2]  := FBR.hgetbits(length0);
              long[3]  := FBR.hgetbits(length0);
              long[4]  := FBR.hgetbits(length0);
              long[5]  := FBR.hgetbits(length0);
          end;

          if ((FSideInfo.ch[ch].scfsi[1] = 0) or (gr = 0)) then begin
              long[6]  := FBR.hgetbits(length0);
              long[7]  := FBR.hgetbits(length0);
              long[8]  := FBR.hgetbits(length0);
              long[9]  := FBR.hgetbits(length0);
              long[10] := FBR.hgetbits(length0);
          end;

          if ((FSideInfo.ch[ch].scfsi[2] = 0) or (gr = 0)) then begin
              long[11] := FBR.hgetbits(length1);
              long[12] := FBR.hgetbits(length1);
              long[13] := FBR.hgetbits(length1);
              long[14] := FBR.hgetbits(length1);
              long[15] := FBR.hgetbits(length1);
          end;

          if ((FSideInfo.ch[ch].scfsi[3] = 0) or (gr = 0)) then begin
              long[16] := FBR.hgetbits(length1);
              long[17] := FBR.hgetbits(length1);
              long[18] := FBR.hgetbits(length1);
              long[19] := FBR.hgetbits(length1);
              long[20] := FBR.hgetbits(length1);
          end;

          long[21] := 0;
          long[22] := 0;
      end;
  end;
end;

// Reads the side info from the stream, assuming the entire
// frame has been read already.

// Mono   : 136 bits (= 17 bytes)
// Stereo : 256 bits (= 32 bytes)
function TLayerIII_Decoder.GetSideInfo: Boolean;
var
  ch, gr: Cardinal;
  gr_info: TGrInfo;
begin
  Assert(FHeader.Version = MPEG1, 'invalid format version');

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
          gr_info.global_gain := FStream.GetBits(8);
          gr_info.scalefac_compress := FStream.GetBits(4);
          gr_info.window_switching_flag := FStream.GetBits(1);
          if (gr_info.window_switching_flag <> 0) then begin
              gr_info.block_type := FStream.GetBits(2);
              gr_info.mixed_block_flag := FStream.GetBits(1);

              gr_info.table_select[0] := FStream.GetBits(5);
              gr_info.table_select[1] := FStream.GetBits(5);

              gr_info.subblock_gain[0] := FStream.GetBits(3);
              gr_info.subblock_gain[1] := FStream.GetBits(3);
              gr_info.subblock_gain[2] := FStream.GetBits(3);

              // Set region_count parameters since they are implicit in this case.
              if (gr_info.block_type = 0) then begin
                  // Side info bad: block_type == 0 in split block
                  Result := False;
                  exit;
              end else if (gr_info.block_type = 2) and (gr_info.mixed_block_flag = 0) then
                  gr_info.region0_count := 8
              else
                  gr_info.region0_count := 7;

              gr_info.region1_count := 20 - gr_info.region0_count;
          end else begin
              gr_info.block_type := 0;
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

  Result := True;
end;

procedure TLayerIII_Decoder.HuffmanDecode(ch: Cardinal; gr: Cardinal);
var i: Cardinal;
    x, y, v, w: Integer;
    part2_3_end: Integer;
    num_bits: Integer;
    region1Start: Cardinal;
    region2Start: Cardinal;
    index: Integer;
    h: PHuffCodeTab;
begin
  part2_3_end := FPart2Start + FSideInfo.ch[ch].gr[gr].part2_3_length;

  // Find region boundary for short block case
  if ((FSideInfo.ch[ch].gr[gr].window_switching_flag <> 0) and (FSideInfo.ch[ch].gr[gr].block_type = 2)) then begin
    // Region2.
    region1Start := 36;   // sfb[9/3]*3=36
    region2Start := 576;  // No Region2 for short block case
  end else begin  // Find region boundary for long block case
    region1Start := sfBandIndex[FSFreq].l[FSideInfo.ch[ch].gr[gr].region0_count + 1];
    region2Start := sfBandIndex[FSFreq].l[FSideInfo.ch[ch].gr[gr].region0_count + FSideInfo.ch[ch].gr[gr].region1_count + 2];  // MI
  end;

  index := 0;
  // Read bigvalues area
  i := 0;
  while (i < (FSideInfo.ch[ch].gr[gr].big_values shl 1)) do begin
    if (i < region1Start) then
      h := @g_hufftables[FSideInfo.ch[ch].gr[gr].table_select[0]]
    else if (i < region2Start) then
      h := @g_hufftables[FSideInfo.ch[ch].gr[gr].table_select[1]]
    else
      h := @g_hufftables[FSideInfo.ch[ch].gr[gr].table_select[2]];

    HuffmanDecoder(h, x, y, v, w, FBR);

    FInputSamples[index] := x;
    FInputSamples[index+1] := y;

    inc(index, 2);
    inc(i, 2);
  end;

  // Read count1 area
  h := @g_hufftables[FSideInfo.ch[ch].gr[gr].count1table_select + 32];
  num_bits := FBR.hsstell;

  while ((num_bits < part2_3_end) and (index < 576)) do begin
    HuffmanDecoder(h, x, y, v, w, FBR);

    FInputSamples[index] := v;
    FInputSamples[index+1] := w;
    FInputSamples[index+2] := x;
    FInputSamples[index+3] := y;

    inc(index, 4);
    num_bits := FBR.hsstell;
  end;

  if (num_bits > part2_3_end) then begin
    FBR.rewindNbits(num_bits - part2_3_end);
    dec(index, 4);
  end;

  num_bits := FBR.hsstell;

  // Dismiss stuffing bits
  if (num_bits < part2_3_end) then
    FBR.hgetbits(part2_3_end - num_bits);

  // Zero out rest
  if (index < 576) then
    FNonZero[ch] := index
  else
    FNonZero[ch] := 576;

  // may not be necessary
  while (index < 576) do begin
    FInputSamples[index] := 0;
    inc(index);
  end;
end;

procedure TLayerIII_Decoder.Hybrid(ch: Cardinal; gr: Cardinal);
var rawout: array[0..35] of Single;
    bt: Cardinal;
    gr_info: PGRInfo;
    tsOut: PSingleArray;
    prvblk: PSingleArray;
    sb18: Cardinal;
begin
  gr_info := @FSideInfo.ch[ch].gr[gr];

  sb18 := 0;
  while (sb18 < 576) do begin
    if (gr_info.window_switching_flag <> 0) and (gr_info.mixed_block_flag <> 0) and (sb18 < 36) then
      bt := 0
    else
      bt := gr_info.block_type;

    tsOut := @FOut_1D[sb18];
    InvMDCT(tsOut, @rawout, bt);

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
  if (gr_info.window_switching_flag <> 0) and (gr_info.block_type = 2) then begin
    for index := 0 to 576-1 do
      FOut_1D[index] := 0.0;
      
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
        FOut_1D[index] := xr1d[reorder_table[FSFreq][index]];
    end;
  end else begin  // long blocks
    for index := 0 to 576-1 do
      FOut_1D[index] := xr1d[index];
  end;
end;

procedure TLayerIII_Decoder.SeekNotify;
begin
  FFrameStart := 0;

  FillChar(FPrevBlock, Sizeof(FPrevBlock), 0);

  FreeAndNil(FBR);
  FBR := TBitReserve.Create;
end;

procedure TLayerIII_Decoder.Stereo(gr: Cardinal);
var sb, ss: Integer;
    is_pos: array[0..575] of Cardinal;
    mode_ext: Cardinal;
    i: Integer;
    ms_stereo, i_stereo: Boolean;
    { IS vars
    is_ratio: array[0..575] of Single;
    max_sfb, sfbcnt, sfb: Integer;
    sfx, i, j, lines, temp, temp2: Integer;
    gr_info: PGRInfo;
    }
begin
  if (FChannels = 1) then begin  // mono , bypass xr[0][][] to lr[0][][]
    for sb := 0 to SBLIMIT-1 do begin
      ss := 0;
      while (ss < SSLIMIT) do begin
        FLR[0][sb][ss]   := FRO[0][sb][ss];
        FLR[0][sb][ss+1] := FRO[0][sb][ss+1];
        FLR[0][sb][ss+2] := FRO[0][sb][ss+2];
        inc(ss, 3);
      end;
    end;
  end else begin
    mode_ext := FHeader.ModeExtension;
    ms_stereo := (FHeader.Mode = JointStereo) and (Mode_Ext and $2 <> 0);
    i_stereo := (FHeader.Mode = JointStereo) and (Mode_Ext and $1 <> 0);
    //gr_info := @FSideInfo.ch[0].gr[gr];
    //io_type := (gr_info.scalefac_compress and 1);  //unused?

    // initialization
    for i := 0 to 576-1 do
      is_pos[i] := 7;

    Assert(i_stereo = false, 'intensity stereo not supported atm');
    { disabled to simplify porting
    if (i_stereo) then begin
      if (gr_info.window_switching_flag <> 0) and (gr_info.block_type = 2) then begin
        if (gr_info.mixed_block_flag <> 0) then begin
          max_sfb := 0;

          for j := 0 to 2 do begin
            sfbcnt := 2;
            sfb := 12;
            while (sfb >= 3) do begin
              i := sfBandIndex[FSFreq].s[sfb];
              lines := sfBandIndex[FSFreq].s[sfb+1] - i;
              i := (i shl 2) - i + (j+1) * lines - 1;

              while (lines > 0) do begin
                if (FRO[1][ss_div[i]][ss_mod[i]] <> 0.0) then begin
                  sfbcnt := sfb;
                  sfb := -10;
                  lines := -10;
                end;

                dec(lines);
                dec(i);
              end;
              
              dec(sfb);
            end;
            sfb := sfbcnt + 1;

            if (sfb > max_sfb) then
              max_sfb := sfb;

            while (sfb < 12) do begin
              temp := sfBandIndex[FSFreq].s[sfb];
              sb := sfBandIndex[FSFreq].s[sfb+1] - temp;
              i := (temp shl 2) - temp + j * sb;

              while (sb > 0) do begin
                is_pos[i] := FScaleFactors[1].short[j][sfb];
                if (is_pos[i] <> 7) then
                  is_ratio[i] := TAN12[is_pos[i]];

                inc(i);
                dec(sb);
              end;
              inc(sfb);
            end;

            sfb := sfBandIndex[FSFreq].s[10];
            sb := sfBandIndex[FSFreq].s[11] - sfb;
            sfb := (sfb shl 2) - sfb + j * sb;
            temp := sfBandIndex[FSFreq].s[11];
            sb := sfBandIndex[FSFreq].s[12] - temp;
            i := (temp shl 2) - temp + j * sb;

            while (sb > 0) do begin
              is_pos[i] := is_pos[sfb];
              is_ratio[i] := is_ratio[sfb];

              inc(i);
              dec(sb);
            end;
          end;

          if (max_sfb <= 3) then begin
            i := 2;
            ss := 17;
            sb := -1;
            while (i >= 0) do begin
              if (FRO[1][i][ss] <> 0.0) then begin
                sb := (i shl 4) + (i shl 1) + ss;
                i := -1;
              end else begin
                dec(ss);
                if (ss < 0) then begin
                  dec(i);
                  ss := 17;
                end;
              end;
            end;

            i := 0;
            while (sfBandIndex[FSFreq].l[i] <= sb) do
              inc(i);

            sfb := i;
            i := sfBandIndex[FSFreq].l[i];
            while (sfb < 8) do begin
              sb := sfBandIndex[FSFreq].l[sfb+1] - sfBandIndex[FSFreq].l[sfb];
              while (sb > 0) do begin
                is_pos[i] := FScaleFactors[1].long[sfb];
                if (is_pos[i] <> 7) then
                  is_ratio[i] := TAN12[is_pos[i]];

                inc(i);
                inc(sb);
              end;
              inc(sfb);
            end;
          end;
        end else begin  // if (gr_info->mixed_block_flag)
          for j := 0 to 2 do begin
            sfbcnt := -1;
            sfb := 12;
            while (sfb >= 0) do begin
              temp := sfBandIndex[FSFreq].s[sfb];
              lines := sfBandIndex[FSFreq].s[sfb+1] - temp;
              i := (temp shl 2) - temp + (j+1) * lines - 1;

              while (lines > 0) do begin
                if (FRO[1][ss_div[i]][ss_mod[i]] <> 0.0) then begin
                  sfbcnt := sfb;
                  sfb := -10;
                  lines := -10;
                end;

                dec(lines);
                dec(i);
              end;
              dec(sfb);
            end;

            sfb := sfbcnt + 1;
            while (sfb < 12) do begin
              temp := sfBandIndex[FSFreq].s[sfb];
              sb := sfBandIndex[FSFreq].s[sfb+1] - temp;
              i := (temp shl 2) - temp + j * sb;
              while (sb > 0) do begin
                // dec(sb);
                is_pos[i] := FScaleFactors[1].short[j][sfb];
                if (is_pos[i] <> 7) then
                  is_ratio[i] := TAN12[is_pos[i]];

                inc(i);
                dec(sb);
              end;

              inc(sfb);
            end;

            temp := sfBandIndex[FSFreq].s[10];
            temp2 := sfBandIndex[FSFreq].s[11];
            sb   := temp2 - temp;
            sfb  := (temp shl 2) - temp + j * sb;
            sb   := sfBandIndex[FSFreq].s[12] - temp2;
            i    := (temp2 shl 2) - temp2 + j * sb;

            while (sb > 0) do begin
              is_pos[i] := is_pos[sfb];
              is_ratio[i] := is_ratio[sfb];

              inc(i);
              dec(sb);
            end;
          end;
        end;
      end else begin  // if (gr_info->window_switching_flag ...
        i := 31;
        ss := 17;
        sb := 0;
        while (i >= 0) do begin
          if (FRO[1][i][ss] <> 0.0) then begin
            sb := (i shl 4) + (i shl 1) + ss;
            i := -1;
          end else begin
            dec(ss);
            if (ss < 0) then begin
              dec(i);
              ss := 17;
            end;
          end;
        end;

        i := 0;
        while (sfBandIndex[FSFreq].l[i] <= sb) do
          inc(i);

        sfb := i;
        i := sfBandIndex[FSFreq].l[i];
        while (sfb < 21) do begin
          sb := sfBandIndex[FSFreq].l[sfb+1] - sfBandIndex[FSFreq].l[sfb];
          while (sb > 0) do begin
            is_pos[i] := FScaleFactors[1].long[sfb];
            if (is_pos[i] <> 7) then
              is_ratio[i] := TAN12[is_pos[i]];

            inc(i);
            dec(sb);
          end;
          inc(sfb);
        end;

        sfb := sfBandIndex[FSFreq].l[20];
        sb := 576 - sfBandIndex[FSFreq].l[21];
        while (sb > 0) and (i < 576) do begin
          is_pos[i] := is_pos[sfb]; // error here : i >=576
          is_ratio[i] := is_ratio[sfb];

          inc(i);
          dec(sb);
        end;
      end;
    end;
    }

    i := 0;
    for sb := 0 to SBLIMIT-1 do
      for ss := 0 to SSLIMIT-1 do begin
        if (is_pos[i] = 7) then begin
          if (ms_stereo) then begin
            FLR[0][sb][ss] := (FRO[0][sb][ss] + FRO[1][sb][ss]) * 0.707106781;
            FLR[1][sb][ss] := (FRO[0][sb][ss] - FRO[1][sb][ss]) * 0.707106781;
          end else begin
            FLR[0][sb][ss] := FRO[0][sb][ss];
            FLR[1][sb][ss] := FRO[1][sb][ss];
          end;
        { disabled to simplify porting
        end else if (i_stereo) then begin
          FLR[1][sb][ss] := FRO[0][sb][ss] / (1 + is_ratio[i]);
          FLR[0][sb][ss] := FLR[1][sb][ss] * is_ratio[i]; }
        end;

        inc(i);
      end;
  end;
end;

end.
