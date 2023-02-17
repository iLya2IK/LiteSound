{
   FrameStreaming example - part of liteSound_ilya2ik

   Copyright 2023 Ilya Medvedkov

   In this example, an audio-ogg file (named cInputFile) is opened and decoded
   into a data stream. The resulting stream is then re-encoded into a set of
   encoded frames. A set of encoded frames is saved to the files on disk  
   (cStreamFile(s)) in the user's format. Frame-files are opened, decoded into
   a data stream and saved in a new file in selected format (cOutputFile).

   step 1.
   cInputFile->AudioDecoder->[pcm]->FramedEncoder->[frames...]->cStreamFile(s)
   step 2.
   cStreamFile(s)->FramedDecoder->[pcm]->AudioEncoder->cOutputFile
}

program framestreaming;

uses
  {$ifdef LINUX}
  cthreads,
  {$endif}
  Classes, SysUtils,
  OGLSoundLite, OGLSoundUtils, OGLSoundUtilTypes;

const // the name of source opus-ogg file
      cInputFile  = '..' + PathDelim + 'media' + PathDelim + 'testing.opus';
      // the name of the intermediate files with encoded frames in user`s format
      cStreamFile = '..' + PathDelim + 'media' + PathDelim + 'frames';
      cFrameE = '.frame';
      // format of audio frames. possible:
      // TSoundLite.CODEC_OPUS
      // TSoundLite.CODEC_OGG_OPUS
      // TSoundLite.CODEC_OGG_VORBIS
      // TSoundLite.CODEC_FLAC
      // TSoundLite.CODEC_OGG_FLAC
      cFrameFormat = TSoundLite.CODEC_OPUS;
      // format of dest reencoded file
      cOutputFileFormat = TSoundLite.CODEC_OGG_VORBIS;
      // the name of dest reencoded file
      cOutputFile = '..' + PathDelim + 'media' + PathDelim + 'output';
      // duration of data chunk to decode/encode
      cDur : Single = 20.0; //ms
      // max duration of single frame
      cMaxFrameDur : Single = 500.0; //ms
      {$ifdef DEBUG}
      cHeapTrace = 'heaptrace.trc';
      {$endif}
const cSL_Components : TSoundLiteComponents = [slcOGG, slcFLAC, slcOpus, slcVorbis];

var
  audiof : TSLTrackFile;    // interface to decode audio file
  oggf   : TSLOutputFile;   // interface to encode audio file
  Files  : TStringList;     // list of frames
  pack_enc : TSLFramedEncoder;   // framed encoder
  pack_dec : TSLFramedDecoder;   // framed decoder

// pop encoded frames from queue and save them to files
procedure PopEncodedFrames;
var
  aFileStream : TFileStream;     // TFileStream linked to cStreamFile
  aFrame : TSLEncodedFrame;
begin
  while true do
  begin
    aFrame := pack_enc.EncodedFrames.PopValue;
    if Assigned(aFrame) then
    begin
      try
        aFileStream := TFileStream.Create(cStreamFile +
                                          Format('%.4d', [aFrame.FrameID]) +
                                          cFrameE, fmOpenWrite or fmCreate);
        try
          aFrame.Position := 0;
          aFileStream.CopyFrom(aFrame, aFrame.Size);
          Files.Add(aFileStream.FileName);
        finally
          aFileStream.Free;
        end;
      finally
        aFrame.Free;
      end;
    end else
      Break;
  end;
end;

// pop decoded frames from queue and encode/write them to output file
procedure PopDecodedFrames;
var
  aFrame : TSLAudioFrame;
begin
  while true do
  begin
    aFrame := pack_dec.DecodedFrames.PopValue;
    if Assigned(aFrame) then
    begin
      try
        aFrame.Position := 0;
        oggf.WriteData(aFrame.Memory, aFrame.FrameSize, nil);
      finally
        aFrame.Free;
      end;
    end else
      Break;
  end;
end;

var i : integer;
    aFileStream : TFileStream;
    Buffer : Pointer;              // intermediate buffer for encoder
    bitrate : Integer;             // current bitrate
    frame_size, max_frame_size : ISoundFrameSize;
    len : ISoundFrameSize;         // length of writed/read data
    aEncProp : ISoundEncoderProps;
begin
  {$ifdef DEBUG}
  if FileExists(cHeapTrace) then
     DeleteFile(cHeapTrace);
  SetHeapTraceOutput(cHeapTrace);
  {$endif}

  // Initialize opus, opusenc, opusfile interfaces - load libraries
  {$ifdef Windows}
  TSoundLite.SetLibPath('..\libs\', cSL_Components);
  {$endif}
  TSoundLite.InitSoundLite(cSL_Components);
  Files := TStringList.Create;
  try
    // Create audio file decoder interface
    audiof := TSLTrackFile.Create;
    try
      if audiof.LoadFromFile(cInputFile, true) then
      begin
        // cInputFile opended and headers/coments are loaded
        // create framesize from cDur
        frame_size := audiof.FileInfo.Codec.FrameFromDuration(cDur);
        max_frame_size := audiof.FileInfo.Codec.FrameFromDuration(cMaxFrameDur);
        // get the file bitrate from audio decoder
        bitrate := audiof.FileInfo.Codec.Bitrate;

        // gen encoder properties
        with TSoundLite do
        aEncProp := TOGLSound.EncProps([ENC_PROP_MODE, oemVBR,
                                        PROP_CHANNELS, audiof.FileInfo.Channels,
                                        PROP_FREQUENCY, audiof.FileInfo.Frequency,
                                        ENC_PROP_BITRATE, bitrate,
                                        ENC_PROP_QUALITY, 0.5,
                                        PROP_SAMPLE_SIZE, audiof.FileInfo.SampleSize,
                                        PROP_CHUNK_SIZE,  frame_size,
                                        PROP_MAX_FRAME_SIZE, max_frame_size]);

        // initialize intermediate buffer to store decoded data chunk
        Buffer := GetMem(frame_size.AsBytes);
        try
          // initialize custom framed encoder
          pack_enc := TSLFramedEncoder.Create(cFrameFormat, aEncProp);
          try
            repeat
              // read decoded pcm data from audio file
              // len - length of decoded data
              len := audiof.ReadData(Buffer, frame_size, nil);

              if len.IsValid then
                pack_enc.WriteData(Buffer, len, nil);

              PopEncodedFrames;
            until len.Less(frame_size);
            // complete the stream formation process.
            // write the packets that are in the cache.
            pack_enc.Close(nil);
            PopEncodedFrames;
          finally
            pack_enc.Free;
          end;
        finally
          FreeMemAndNil(Buffer);
        end;
      end;
    finally
      audiof.Free;
    end;

    oggf := TSLOutputFile.Create;
    try
      if oggf.SaveToFile(cOutputFile + TSoundLite.GetFileExt(cOutpuFileFormat),
                                     cOutputFileFormat, aEncProp, nil) then
      begin
        pack_dec := TSLFramedDecoder.Create(cFrameFormat, aEncProp);
        try
          for i := 0 to Files.Count-1 do
          begin
            aFileStream := TFileStream.Create(Files[i], fmOpenRead);
            try
              pack_dec.DataSource := aFileStream;
              pack_dec.DecodeAllData(nil);
              pack_dec.Flush;
              PopDecodedFrames;
            finally
              aFileStream.Free;
            end;
          end;
          oggf.StopStreaming;

        finally
          pack_dec.Free;
        end;
      end;
    finally
      oggf.Free;
    end;
  finally
    Files.Free;
  end;
    // close soundlite interfaces
  TSoundLite.DoneSoundLite;
end.

