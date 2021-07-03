(*
 *  File:     $RCSfile: L3Tables.pas,v $
 *  Revision: $Revision: 1.1.1.1 $
 *  Version : $Id: L3Tables.pas,v 1.1.1.1 2002/04/21 12:57:21 fobmagog Exp $
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
unit L3Tables;

interface

uses
  math;

const
  slen: array[0..1, 0..15] of uint8 = (
    (0, 0, 0, 0, 3, 1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4),
    (0, 1, 2, 3, 0, 1, 2, 3, 1, 2, 3, 1, 2, 3, 2, 3));

  pretab: array[0..21] of uint8 =
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 3, 3, 3, 2, 0);

  sfBandIndex: array[0..2] of packed record
    l: array[0..22] of uint16;
    s: array[0..13] of uint8;
  end =
    ((l: (0,4,8,12,16,20,24,30,36,44,52,62,74,90,110,134,162,196,238,288,342,418,576);
      s: (0,4,8,12,16,22,30,40,52,66,84,106,136,192)),
     (l: (0,4,8,12,16,20,24,30,36,42,50,60,72,88,106,128,156,190,230,276,330,384,576);
      s: (0,4,8,12,16,22,28,38,50,64,80,100,126,192)),
     (l: (0,4,8,12,16,20,24,30,36,44,54,66,82,102,126,156,194,240,296,364,448,550,576);
      s: (0,4,8,12,16,22,30,42,58,78,104,138,180,192)));

  two_to_negative_half_pow: array[0..63] of Single = (
    1.0000000000E+00, 7.0710678119E-01, 5.0000000000E-01, 3.5355339059E-01,
    2.5000000000E-01, 1.7677669530E-01, 1.2500000000E-01, 8.8388347648E-02,
    6.2500000000E-02, 4.4194173824E-02, 3.1250000000E-02, 2.2097086912E-02,
    1.5625000000E-02, 1.1048543456E-02, 7.8125000000E-03, 5.5242717280E-03,
    3.9062500000E-03, 2.7621358640E-03, 1.9531250000E-03, 1.3810679320E-03,
    9.7656250000E-04, 6.9053396600E-04, 4.8828125000E-04, 3.4526698300E-04,
    2.4414062500E-04, 1.7263349150E-04, 1.2207031250E-04, 8.6316745750E-05,
    6.1035156250E-05, 4.3158372875E-05, 3.0517578125E-05, 2.1579186438E-05,
    1.5258789062E-05, 1.0789593219E-05, 7.6293945312E-06, 5.3947966094E-06,
    3.8146972656E-06, 2.6973983047E-06, 1.9073486328E-06, 1.3486991523E-06,
    9.5367431641E-07, 6.7434957617E-07, 4.7683715820E-07, 3.3717478809E-07,
    2.3841857910E-07, 1.6858739404E-07, 1.1920928955E-07, 8.4293697022E-08,
    5.9604644775E-08, 4.2146848511E-08, 2.9802322388E-08, 2.1073424255E-08,
    1.4901161194E-08, 1.0536712128E-08, 7.4505805969E-09, 5.2683560639E-09,
    3.7252902985E-09, 2.6341780319E-09, 1.8626451492E-09, 1.3170890160E-09,
    9.3132257462E-10, 6.5854450798E-10, 4.6566128731E-10, 3.2927225399E-10);


type
  TSampleFrequency = (SampleFreq_44p1 = 0, SampleFreq_48, SampleFreq_32, Unknown);

var
  //table for power of 4/3. Top index is the largest huff encoded value for maximum linbits (13): 15 + (1 << 13 - 1)
  L3_pow_43: array[0..8206] of Single;

  L3_short_reorder_table: array[0..2, 0..575] of uint16;
  L3_global_gain_table: array[byte] of single;

procedure InitReorderTable;
procedure InitPower43Table;
procedure InitGlobalGainTable;

implementation

procedure InitReorderTable;
var
  i, j: integer;
  freq_idx: integer;
  index, sfb, window: integer;
  istart, iend: UInt8;
begin
  for freq_idx := 0 to 2 do begin
      j := 0;
      for sfb := 0 to 12 do begin
          istart := sfBandIndex[freq_idx].s[sfb];
          iend   := sfBandIndex[freq_idx].s[sfb+1];
          for window := 0 to 2 do
              for i := istart to iend - 1 do begin
                  index := 3 * i + window;
                  L3_short_reorder_table[freq_idx, index] := j;
                  j += 1;
              end;
      end;
  end;
end;

procedure InitPower43Table;
var
  i: Integer;
begin
  L3_pow_43[0] := 0;
  for i := 1 to High(L3_pow_43) do begin
      L3_pow_43[i] := Power(i, 4/3);
  end;
end;

procedure InitGlobalGainTable;
var
  i: integer;
begin
  for i := 0 to 255 do
      L3_global_gain_table[i] := Power(2.0 , (0.25 * (i - 210.0)));
end;

end.
