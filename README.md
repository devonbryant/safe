Safe
====

Safe is a system for scalable and efficient audio feature extraction.  Safe is primarily useful for running bulk extraction on large sets of audio data.  The [Performance](http://github.com/devonbryant/safe/wiki/Performance) page shows how Safe compares with other audio feature extraction libraries. For a more detailed description of the system and the motivation behind it, see the [Scalable Audio Feature Extraction](http://devonbryant.github.io/thesis/final/safe.pdf) thesis paper.

Running
-------

Safe is divided into two projects:

 * _safe-core_ - The core library, feature extraction algorithms, and a command-line interface.
 * _safe-dist_ - Provides cluster capabilities and a web-interface for feature extraction.

To build and run Safe, you will need to have [SBT](http://www.scala-sbt.org/) installed.  The following command will compile Safe and generate scripts for running the system:

```
sbt start-script
```

The simplest way to run Safe is through the command-line interface.  Safe provides the following command-line options:

```
  -i <file> | --input <file>
        input path (file or directory) for audio file(s) to process
  -r | --recursive
        flag to process audio data in sub-directories (default = false)
  -p <file> | --plan <file>
        path to feature extraction plan file
  -f <value> | --feature <value>
        description and parameters for a single feature to extract
  -s <value> | --sample-rate <value>
        sample rate in Hz of all audio files (default = 44100)
  -o <file> | --output-dir <file>
        directory to write featre output to (default = './')
  -m | --metrics
        flag to capture/print metrics (default = false)
```

In order to run the system, an input audio file (or directory containing audio files) ```-i``` and a description of the feature(s) (```-f``` or ```-p```) to be extracted are required.  Features are described in the following format (where ```<parameters>``` are a comma-separated list of ```param=value``` options):

```name: Feature [ <parameters> ] [ -out <parameters> ]```

```name``` is a unique identifier that will be used in the name of the output file.  ```Feature``` is the system identifier for the feature to be extracted.  The following are some example descriptions of features:

```
my_mfcc: MFCC frameSize=512, stepSize=256, windowType="hamming"
my_flux: SpectralFlux
my_shape: SpectralShape -out precision=2
```

The following example will extract the Mel-Frequency Cepstral Coefficients from an audio file "/home/safe/music/song.wav" and write the results to "/home/safe/out/song.wav.mfcc.csv":

```
safe-core/target/run -i "/home/safe/music/song.wav" -f "mfcc: MFCC" -o "/home/safe/out"
```

See [Existing Features](http://github.com/devonbryant/safe/wiki/Existing-Features) for a full description of the current feature extraction algorithms and parameters.

Safe currently only works with PCM-encoded audio files (WAV, AIFF, etc.).  Future versions will support more encodings such as MP3, OGG, etc.

License
-------

This software is licensed under the Eclipse Public License 1.0 (http://www.eclipse.org/org/documents/epl-v10.html)

Sponsors
--------

[Yourkit](http://www.yourkit.com/java/profiler/index.jsp) - Kindly supporting open source projects with its full-featured JVM Profiler
