program mp3player_demo;

uses
  sysutils, classes,
  sdl2, display, glad_gl, fpimgui, fpimgui_impl_sdlgl2,
  mp3stream;

var
  g_state: record
      audio_device_id: integer;
      data: pbyte;
      size: integer;
      stream_position: integer;
      volume: integer;
      memstream: TMemoryStream;
  end;

procedure DrawUI;
var
  sample_bytes: byte;
begin
  ImGui.Begin_('MP3play');
    if ImGui.Button('Pause') then begin
        SDL_PauseAudioDevice(g_state.audio_device_id, 1);
    end;
    Imgui.SameLine();
    if ImGui.Button('Play') then begin
        SDL_PauseAudioDevice(g_state.audio_device_id, 0);
    end;
    sample_bytes := 4; //int16 size, 2 channels
    ImGui.Text(format('sample %d / %d', [g_state.stream_position div sample_bytes, g_state.size div sample_bytes]));
    Imgui.SliderInt('Volume', @g_state.volume, 0, 128);

  ImGui.End_;
end;

procedure AudioCallback(userdata: Pointer; stream: PUInt8; len: Integer) cdecl;
var
  src: pbyte;
begin
  src := g_state.data + g_state.stream_position;
  //move(src^, stream^, len);
  FillByte(stream^, len, 0);
  SDL_MixAudioFormat(stream, src, AUDIO_S16, len, g_state.volume);

  g_state.stream_position += len;
end;

procedure AudioInit;
var
  desired, obtained: TSDL_AudioSpec;
  num_audio_devices: integer;
  i: integer;
begin
  num_audio_devices := SDL_GetNumAudioDevices(0 {isCapture});
  if num_audio_devices > 0 then
      for i := 0 to num_audio_devices - 1 do
          writeln(SDL_GetAudioDeviceName(i, 0 {isCapture}));

  desired.freq := 44100;  //TODO must synchronize with freq in mp3 file, always use 44.1kHz for now
  desired.format := AUDIO_S16;
  desired.channels := 2;
  desired.samples := 4096;
  desired.callback := @AudioCallback;
  desired.userdata := nil;

  g_state.audio_device_id := SDL_OpenAudioDevice(nil, 0, @desired, @obtained, 0);
  if g_state.audio_device_id = 0 then begin
      writeln(SDL_GetError);
      halt;
  end;
  Assert(obtained.size = 2 {channels} * 2 {sample size} * desired.samples);
end;

procedure DecodeMP3;
const
  BASEPATH = '..\samples\';
  MP3_FILE = BASEPATH + 'bensound-happyrock.mp3';
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

  inputStream.Free;

  //ms.SaveToFile('out_ms2.raw');

  g_state.data := ms.Memory;
  g_state.size := ms.Position;
  g_state.memstream := ms;

  writeln(g_state.size);
end;


var
  disp: TDisplay;
  ev: TSDL_Event;

begin
  //init globals
  with g_state do begin
      audio_device_id := 0;
      stream_position := 0;
      data := nil;
      size := 0;
      volume := SDL_MIX_MAXVOLUME;
  end;

  //load and decode mp3
  DecodeMP3;
  writeln('music loaded');

  //open new SDL window with OpenGL rendering support
  SDL_Init(SDL_INIT_VIDEO or SDL_INIT_TIMER or SDL_INIT_AUDIO);
  disp := TDisplay.Create;
  disp.InitDisplay(640, 240);

  //init audio
  AudioInit;

  // start audio playing
  SDL_PauseAudioDevice(g_state.audio_device_id, 0);
  while true do begin
      disp.NewFrame;

      DrawUI;
      ImGui.Render;

      disp.PresentFrame;
      Assert(glGetError() = GL_NO_ERROR);

      //stop if whole buffer was sent
      if g_state.stream_position >= g_state.size - 1 then
          break;

      //handle input
      if SDL_PollEvent(@ev) <> 0 then begin
          //pass events to imgui as well, otherwise widgets wouldn't be interactive
          ImGui_ImplSdlGL2_ProcessEvent(@ev);

          case ev.type_ of
                SDL_QUITEV:
                    break;
                //(other event handling)
          end;
      end;
  end;

  //shutdown audio
  SDL_CloseAudioDevice(g_state.audio_device_id);
  g_state.memstream.Free;

  //we won't need the SDL window anymore
  disp.FreeDisplay;
  disp.Free;
  SDL_Quit;
end.



