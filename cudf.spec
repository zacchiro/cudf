Summary: CUDF (Common Upgradeability Description Format) tools.
Name: cudf
Version: 0.5.94
Release: 1
Source: http://gforge.info.ucl.ac.be/frs/download.php/156/cudf-0.5.94.tar.gz
URL: http://www.mancoosi.org/cudf/
License: LGPL
Group: Development/Libraries
BuildRequires: ocaml ocaml-findlib ocaml-camlp4-devel ocaml-extlib-devel
BuildRoot: %{_tmppath}/%{name}-root

%description
CUDF (for Common Upgradeability Description Format) is a format for describing
upgrade scenarios in package-based Free and Open Source Software distribution.

libCUDF is a library to manipulate so called CUDF documents. A CUDF document
describe an upgrade problem, as faced by package managers in popular
package-based GNU/Linux distributions.

%package devel
Summary: CUDF (Common Upgradeability Description Format) development tools.

%description devel
CUDF (for Common Upgradeability Description Format) is a format for describing
upgrade scenarios in package-based Free and Open Source Software distribution.

libCUDF is a library to manipulate so called CUDF documents. A CUDF document
describe an upgrade problem, as faced by package managers in popular
package-based GNU/Linux distributions.

This package contains the development stuff needed to use libCUDF in your
programs.

%prep
%setup -q

%build
make all c-lib
which /usr/bin/ocamlopt > /dev/null && make opt c-lib-opt

%install
rm -rf $RPM_BUILD_ROOT
make DESTDIR="$RPM_BUILD_ROOT" install

%check
make test

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root)
%{_bindir}/cudf-check
%{_bindir}/cudf-parse-822
%{_libdir}/ocaml/cudf

%files devel
%defattr(-,root,root)
%{_includedir}/cudf.h
%{_libdir}/*.a
%{_libdir}/pkgconfig/cudf.pc

%changelog
* Sat Dec 19 2009 Stefano Zacchiroli <zack@pps.jussieu.fr>
- various adjustments (deps, description, ...)

* Fri Dec 18 2009 Jeff Johnson <jbj@rpm5.org>
- create.
