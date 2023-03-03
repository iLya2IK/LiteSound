{
   Resampler example - part of liteSound_ilya2ik

   Copyright 2023 Ilya Medvedkov

   This example demonstrates the operation of a simple speex resampler.
}

program resampler;

uses OGLSoundLite, OGLSoundUtils, OGLSoundUtilTypes, Math;

const
  CHANNELS = 2;          // number of channels
  FREQ_IN  = 44100;      // input rate
  FREQ_OUT = 48000;      // output rate
  DURATION = 120;        // duration of data frame in ms
  QUALITY  = 8;          // quality value for resampling

var
  InpBuffer, OutBuffer   : PSmallInt;

  aResampler             : ISoundResampler;

  aFrame, aOutFrame, len : ISoundFrameSize;

  aSamplesCount          : integer;
  aLstPos, aLen          : Int64;

  OutF                   : TextFile;

  i, j                   : integer;
begin
  { Create an incoming audio frame with the specified characteristics and duration }
  aFrame    := TOGLSound.FrameFromDuration(FREQ_IN,  CHANNELS, ss16bit, DURATION);
  { Create an outgoing audio frame with the specified characteristics and duration }
  aOutFrame := TOGLSound.FrameFromDuration(FREQ_OUT, CHANNELS, ss16bit, DURATION);

  { Allocate the memory needed to store audio frames of a given duration }
  InpBuffer := GetMem(aFrame.AsBytes);
  OutBuffer := GetMem(aOutFrame.AsBytes);
  try
    { Fill in the values in the input data buffer.
      A sine wave is set in one channel, a cosine is set in the other. }
    aSamplesCount := aFrame.AsSamples;
    for i := 0 to aSamplesCount-1 do
    begin
      InpBuffer[i*CHANNELS]   := Round(32767.0 * Cos(double(i) / aSamplesCount * 8.0 * Pi));
      InpBuffer[i*CHANNELS+1] := Round(32767.0 * Sin(double(i) / aSamplesCount * 8.0 * Pi));
    end;

    { Initializing the resampler. }
    aResampler :=
      TSoundLite.NewSpeexResampler(aOutFrame,
                                   FREQ_IN,
                                   QUALITY,
                                   TOGLSound.Props(
                                   {set
                                    PROP_SPEEX_SAMPLER_SKIP_ZEROS = true
                                    if you don't need to record multiple zero
                                    values that are the results of the speex
                                    algorithm. these zeros can be skipped to
                                    align the output data with the input data.}
                                   [TSoundLite.PROP_SPEEX_SAMPLER_SKIP_ZEROS, true]));
    { Send the entire input data to the resampler }
    aResampler.WriteInterleave(InpBuffer, aFrame);
    { Write the result of the resampler operation in the receiving buffer }
    aLstPos := aResampler.OutBufferSize;
    Move(aResampler.OutBuffer^, OutBuffer^, aLstPos);
    { If the receiving buffer is not fully filled, write the extrapolation
      result (it may look like an arbitrary signal defined by a polynomial
      approximation). These several dozen samples are an artifact of the speex
      library algorithm. This method of extracting "residual" data can be used
      if it is necessary to record an audio frame with a duration equal to the
      duration of the original frame. }
    if aLstPos < aOutFrame.AsBytes then
    begin
      len := aResampler.Flush;
      if len.IsValid then
      begin
        aLen := aResampler.OutBufferSize;
        if aLen > (aOutFrame.AsBytes - aLstPos) then
          aLen := (aOutFrame.AsBytes - aLstPos);
        Move(aResampler.OutBuffer^, PByte(OutBuffer)[aLstPos], aLen);
      end;
    end;

    { Write the result of the resampler's work to the output file. }
    Assign(OutF, 'output.txt');
    Rewrite(OutF);
    for i := 0 to aSamplesCount-1 do
    begin
      j := FREQ_OUT * i div FREQ_IN;
      WriteLn(OutF, InpBuffer[i*CHANNELS], #9, InpBuffer[i*CHANNELS+1], #9,
                    OutBuffer[j*CHANNELS], #9, OutBuffer[j*CHANNELS+1]);
    end;
    CloseFile(OutF);
  finally
    Freemem(InpBuffer);
    Freemem(OutBuffer);
  end;
end.

