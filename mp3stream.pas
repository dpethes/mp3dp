unit mp3stream;

interface
uses
  sysUtils, classes,
  Layer3, Header, BitStream, L3Tables;

type
  TDecodedSlice = record
      data: pbyte;           //decoded samples
      size: integer;         //decoded data size in bytes
      frame_bytes: integer;  //how many bytes of input have been consumed
  end;

  TMP3StreamInfo = record
      layer: byte;
      channels: byte;
      freq_khz: byte;
      frame_count: int32;
      length: int32;
      error: string;
  end;

  { TMP3Stream }

  TMP3Stream = class
  private
    FLayer3: TLayerIII_Decoder;

  public
    constructor Create;
    procedure DecodeFile(input: TStream; Output: TStream);
  end;

implementation


{ TMP3Stream }

constructor TMP3Stream.Create;
begin
  InitPower43Table;
  InitReorderTable;
end;

procedure TMP3Stream.DecodeFile(input: TStream; Output: TStream);
var
  has_frame: Boolean;
  frame_count: integer;
  stream: TBitStream;
  header: THeader;
begin
  stream := TBitStream.Create(input);
  header := THeader.Create();
  header.ReadHeader(stream);

  FLayer3 := TLayerIII_Decoder.Create(stream, header, Output);

  has_frame := True;
  frame_count := 0;
  while has_frame do begin
      if (header.Layer <> 3) or (header.Version <> MPEG1) then begin
          //set error?
          break;
      end;
      FLayer3.DecodeSingleFrame;
      has_frame := header.ReadHeader(stream);
      frame_count += 1;
  end;

  FreeAndNil(FLayer3);
  header.Free;
  stream.Free;
end;



end.
