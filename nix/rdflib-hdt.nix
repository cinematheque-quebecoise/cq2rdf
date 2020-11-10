{ buildPythonPackage, fetchPypi, lib, rdflib, pybind11 }:

buildPythonPackage rec {
  pname = "rdflib_hdt";
  version = "3.0";

  src = fetchPypi {
    inherit pname version;
    sha256 = "1d3xqiw94snr9gyjq0r9z7yhxdqhyf11l6ibdq3rihnvjab00jh6";
  };

  # nativeBuildInputs = [ nose ];
  propagatedBuildInputs = [ rdflib pybind11 ];

  doCheck = false;

  meta = with lib; {
    homepage = "https://github.com/RDFLib/rdflib-hdt";
    license = licenses.mit;
    description = "a store back-end for rdflib to allow for reading and querying HDT documents";
    maintainers = [ maintainers.koslambrou ];
  };
}

