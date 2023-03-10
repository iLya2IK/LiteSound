{
   fft example - part of liteSound_ilya2ik

   Copyright 2023 Ilya Medvedkov

   This example demonstrates the operation of a simple fast discrete Fourier
   transform and inverse.
}

program fft_test;

uses OGLSoundLite, OGLSoundUtils, OGLSoundUtilTypes,
     Math, sysutils;

var
  N, i, j, NB : integer;

  X, f, p, a : pSingle;
  amp, arg : Single;
  harm : TSoundComplexData;

  fft_f : ISoundForwardTransformation;
  fft_b : ISoundBackwardTransformation;

  TF : TextFile;
begin
  Randomize;

  // demonstration based on 1024 signal samples
  N := 1024;
  // the number of random components in the signal
  NB := 6;

  // memory allocation for a random signal and its random constituents
  X := pSingle(AllocMem(N*sizeof(Single)));
  f := pSingle(AllocMem(NB*sizeof(Single)));
  p := pSingle(AllocMem(NB*sizeof(Single)));
  a := pSingle(AllocMem(NB*sizeof(Single)));

  // generating random constituents
  for j := 0 to NB-1 do
  begin
    f[j] := (Random - Random) * Pi / 2.0;
    p[j] := Random(N div 2)+1;
    a[j] := Random * 5 + 5;
  end;

  // generating a random signal
  for i := 0 to N-1 do
  begin
    X[i] := 0;
    for j := 0 to NB-1 do
      X[i] := X[i]  + a[j] * cos(2.0 * Pi * i * p[j] / N + f[j]);
  end;

  // create an interface for direct fast fourier transform
  fft_f := TSoundLite.NewForwardFFT(N, 1);
  // convert the original random signal
  fft_f.ProcessInterleave(X, ssFloat);

  // create an interface for inverse fast fourier transform
  fft_b := TSoundLite.NewBackwardFFT(N, 1);
  // convert the spectrum from fft_f to an audio signal
  fft_b.Process(fft_f.OutputRaw);

  // create file to save original random constituents and spectrum
  AssignFile(tf, 'output_spectrum.txt');
  Rewrite(tf);

  WriteLn(tf, 'original');

  // original components
  for i := 0 to NB-1 do
  begin
    WriteLn(tf, Format('%g; %g; %g', [a[i], p[i], f[i]]));
  end;

  WriteLn(tf, 'FT');

  // ft components
  for i := 0 to N div 2-1 do
  begin
    // get the i-th harmonic
    harm := fft_f.OutputHarmonic(0, i);
    // calc the amplitude for the i-th harmonic
    amp := sqrt(sqr(harm.r) + sqr(harm.i)) * 2.0 / single(N);

    // calc the argument for the i-th harmonic
    if amp > 1.0 then
    begin
      if harm.r > 0 then
        arg := ArcTan(harm.i / harm.r) else
      if harm.r < 0 then
      begin
        if (harm.i >= 0) then
          arg := Pi + ArcTan(harm.i / harm.r) else
        if (harm.i < 0) then
          arg := -Pi + ArcTan(harm.i / harm.r);
      end else
      begin
        if (harm.i > 0) then
          arg := Pi/2.0 else
        if (harm.i < 0) then
          arg := -Pi/2.0;
      end;

      WriteLn(tf, Format('%g; %d; %g', [amp, i, arg]));
    end;
  end;

  CloseFile(tf);

  // create file to save the original signal and an restored signal
  AssignFile(tf, 'output.txt');
  Rewrite(tf);

  for i := 0 to N-1 do
  begin
    WriteLn(tf, X[i], '; ', pSingle(fft_b.Output[0])[i] / Single(N));
  end;

  CloseFile(tf);

  Freemem(f);
  Freemem(p);
  Freemem(a);

  Freemem(X);
end.

