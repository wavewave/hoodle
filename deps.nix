{ fetchFromGitHub }:
{
  poppler = fetchFromGitHub {
    owner = "wavewave";
    repo = "poppler";
    rev = "0a9ea94415c80595f8ebd7bcb5069493da447996";
    sha256 = "1f3c4psavxz3jxhwzcgjfa34jly2vf7ln202x38zp98claw6s70v";
  };

  pdf-toolbox = fetchFromGitHub {
    owner = "wavewave";
    repo = "pdf-toolbox";
    rev = "df9182d2dabc7cfe90b13e43b6a43332f852f23f";  # stable-build-fix branch
    sha256 = "1l170fhgblcd2fvaag35xyrnzrz5g5qcal2k3v7nx2rcjij5yw6a";
  };
}
