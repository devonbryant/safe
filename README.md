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

Safe currently only works with PCM-encoded audio files (WAV, AIFF, etc.).  Future versions will support more encodings such as MP3, OGG, etc.

Features
--------

Safe currently provides the following feature extraction algorithms and parameters:

 1. **CQT** - Constant-Q Transform [1]
   * _sampleRate_ - Target (expected) sample rate of audio inputs (default = 44100)
   * _stepSize_ - The step size (number of samples) for the framing function (default = 512)
   * _windowType_ - Windowing function: bartlett, blackman, blackmanHarris, hamming, or hann (default = hann)
   * _binsPerOctave_ - The number of CQT bins/octave (default = 24)
   * _maxFreq_ - Maximum frequency (hz) to look for (default = 12543.854)
   * _minFreq_ - Minimum frequency (hz) to look for (default = 16.351599)
   * _threshold_ - Minimum threshold (default = 0.0054)

 2. **MFCC** - Mel-Frequency Cepstral Coefficients [2]
   * _sampleRate_ - Target (expected) sample rate of audio inputs (default = 44100)
   * _frameSize_* - Frame size (number of samples) for the framing fucntion (default = 1024)
   * _stepSize_ - The step size (number of samples) for the framing function (default = 512)
   * _windowType_ - Windowing function: bartlett, blackman, blackmanHarris, hamming, or hann (default = hann)
   * _numCoeffs_ - Number of cepstral coefficients to extract (default = 13)
   * _melFilters_ - Number of mel filter banks to use (default = 40)
   * _minFreq_ - Minimum frequency (hz) for the filter bank (default = 130.0)
   * _maxFreq_ - Maximum frequency (hz) for the filter bank (default = 6854.0)

 3. **SpectralShape** - A combination of four spectral features: Centroid, Spread, Skewness, and Kurtosis [3]
   * _sampleRate_ - Target (expected) sample rate of audio inputs (default = 44100)
   * _frameSize_* - Frame size (number of samples) for the framing fucntion (default = 1024)
   * _stepSize_ - The step size (number of samples) for the framing function (default = 512)
   * _windowType_ - Windowing function: bartlett, blackman, blackmanHarris, hamming, or hann (default = hann)

 4. **SpectralFlux** - Spectral Flux [4]
   * _sampleRate_ - Target (expected) sample rate of audio inputs (default = 44100)
   * _frameSize_* - Frame size (number of samples) for the framing fucntion (default = 1024)
   * _stepSize_ - The step size (number of samples) for the framing function (default = 512)
   * _windowType_ - Windowing function: bartlett, blackman, blackmanHarris, hamming, or hann (default = hann)
   * _diffLength_ - Compares frames space _n_ length apart, 1 = consecutive frames (default = 1)

 5. **SpectralOnsets** - Spectral Onset Detection [5]
   * _sampleRate_ - Target (expected) sample rate of audio inputs (default = 44100)
   * _frameSize_* - Frame size (number of samples) for the framing fucntion (default = 1024)
   * _stepSize_ - The step size (number of samples) for the framing function (default = 512)
   * _windowType_ - Windowing function: bartlett, blackman, blackmanHarris, hamming, or hann (default = hann)
   * _ratio_ - Minimum activation ratio for windowing function (default = 0.22)
   * _threshold_ - Minimum threshold for peak-picking (default = 2.5)

 > [1] Judith C Brown and Miller S Puckette. An efficient algorithm for the calculation of a constant q transform. The Journal of the Acoustical Society of America, 92:2698, 1992.<br/>
 > [2] Steven Davis and Paul Mermelstein. Comparison of parametric representations for monosyllabic word recognition in continuously spoken sentences. Acoustics, Speech and Signal Processing, IEEE Transactions on, 28(4):357–366, 1980.<br/>
 > [3] Olivier Gillet and Ga ̈el Richard. Automatic transcription of drum loops. In
Acoustics, Speech, and Signal Processing, 2004. Proceedings.(ICASSP’04). IEEE International Conference on, volume 4, pages iv–269. IEEE, 2004.<br/>
 > [4] Simon Dixon. Onset detection revisited. In Proceedings of the 9th International Conference on Digital Audio Effects, volume 120, pages 133–137, 2006.<br/>
 > [5] Sebastian Bock, Florian Krebs, and Markus Schedl. Evaluating the online capa- bilities of onset detection methods. In ISMIR, pages 49–54, 2012.

License
-------

This software is licensed under the Eclipse Public License 1.0 (http://www.eclipse.org/org/documents/epl-v10.html)

Sponsors
--------

[Yourkit](http://www.yourkit.com/java/profiler/index.jsp) - Kindly supporting open source projects with its full-featured JVM Profiler
