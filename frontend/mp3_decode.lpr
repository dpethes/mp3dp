program mp3_decode;

uses
  sysutils, classes,
  mp3stream;

procedure DecodeMP3;
const
  BASEPATH = '..\samples\';
  MP3_FILE = BASEPATH + 'bensound-happyrock.mp3';
  OUT_FILE = 'out_sample.raw';
var
  mp3: TMP3Stream;
  inputStream: TFileStream;
  ms: TMemoryStream;
begin
  inputStream := TFileStream.Create(MP3_FILE, fmOpenRead or fmShareDenyNone);

  ms := TMemoryStream.Create;
  ms.SetSize(1 shl 20);  //reserve at least 1MB output

  mp3 := TMP3Stream.Create;
  mp3.DecodeFile(inputStream, ms);
  mp3.Free;

  ms.SaveToFile(OUT_FILE);
  ms.Free;
  inputStream.Free;
end;


begin
  //load and decode mp3
  DecodeMP3;
end.



