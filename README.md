# liteSound_iLya2IK

A set of classes for playing, recording and converting audio files and audio streams. In addition, the package contains classes and interfaces for working with resamplers, spectrum extractors and audio signal filtering. Uses the principles of dynamic linking of libraries. Supports audio formats WAV, FLAC, Vorbis and Opus (including for the organization of streaming audio frames).

### Examples

* Player example - This example emulates the operation of a simple audio player, demonstrates working with comments, metadata, playlists.
![Player example](https://github.com/iLya2IK/LiteSound/blob/main/examples/screenshots/Screenshot_20230318_144642.png?raw=true)
* Recorder example - This example emulates the operation of a simple audio recorder.
![Player example](https://github.com/iLya2IK/LiteSound/blob/main/examples/screenshots/Screenshot_20230318_144908.png?raw=true)
* Converter example - This example emulates the operation of a simple audio file converter.
![Player example](https://github.com/iLya2IK/LiteSound/blob/main/examples/screenshots/Screenshot_20230318_145047.png?raw=true)
* FrameStreaming example - In this example, an audio-ogg file is opened and decoded into a data stream. The resulting stream is then re-encoded into a set of encoded frames. A set of encoded frames is saved to the files on disk in the user's format. Frame-files are then opened, decoded into a data stream and saved in a new file in selected format.
* Resampler - This example demonstrates the operation of a simple speex resampler.
* fft - This example demonstrates the operation of a simple fast discrete Fourier transform and inverse. 
* Spectre - This example shows a visualization of a fast discrete Fourier transform for an audio stream using OpenGL.
![Player example](https://github.com/iLya2IK/LiteSound/blob/main/examples/screenshots/Screenshot_20230318_145201.png?raw=true)
* WCRadioClient example - An example of the implementation of an Internet radio source and receiver.
![Player example](https://github.com/iLya2IK/LiteSound/blob/main/examples/screenshots/Screenshot_20230318_145652.png?raw=true)
![Player example](https://github.com/iLya2IK/LiteSound/blob/main/examples/screenshots/Screenshot_20230318_145730.png?raw=true)
 
### Requirements

* [Free Pascal Compiler](http://freepascal.org)
* [Lazarus IDE](http://www.lazarus.freepascal.org/) (optional / for examples)
* [CommonUtils](https://github.com/iLya2IK/commonutils)
* [SoundUtils](https://github.com/iLya2IK/SoundUtils)
* [libOpenALsoft_ilya2ik - free pascal bindings and wrapper around OpenAL library](https://github.com/iLya2IK/libOpenALsoft_dyn) 
* [libOGG_ilya2ik - free pascal bindings and wrapper around OGG library](https://github.com/iLya2IK/libOGG_litedyn)
* [libFLAC_ilya2ik - free pascal bindings and wrapper around FLAC library](https://github.com/iLya2IK/libFLAC_litedyn) 
* [libVorbis_ilya2ik - free pascal bindings and wrapper around Vorbis library](https://github.com/iLya2IK/libVorbis_litedyn) 
* [libOpus_ilya2ik - free pascal bindings and wrapper around Opus library](https://github.com/iLya2IK/libOpus_litedyn) 
* [HTTP2 Web Camera Client](https://github.com/iLya2IK/wccurlclient) (optional / for WCRadioClient example)
* [REST Web Camera server](https://github.com/iLya2IK/wcwebcamserver) (optional / for WCRadioClient example)

### Third party

* [OpenAL soft](https://openal-soft.org/)
* [OGG](https://xiph.org/ogg/)
* [libFLAC](https://xiph.org/flac/download.html)
* [Vorbis/VorbisEnc/VorbisFile](https://xiph.org/vorbis/)
* [Opus/OpusEnc/OpusFile](https://opus-codec.org/downloads/)
* [OpenGL v3.3](https://www.opengl.org/)(optional - for Spectre example)

### Installation

Get the sources and add the *src* directory to the project search path. For the Lazarus development environment, you can install the *litesound_ilya2ik.lpk* package for your project. For FPC add the *src* directory to the *fpc.cfg* file.

### Copyrights

* Copyright (c) 2023, Ilya Medvedkov
