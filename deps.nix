{ fetchFromGitHub }:
{
  poppler = fetchFromGitHub {
    owner = "wavewave";
    repo = "poppler";
    rev = "0a9ea94415c80595f8ebd7bcb5069493da447996";
    sha256 = "1f3c4psavxz3jxhwzcgjfa34jly2vf7ln202x38zp98claw6s70v";
  };

  pdf-toolbox = /home/wavewave/repo/srcc/pdf-toolbox;
}
