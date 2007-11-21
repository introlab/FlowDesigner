%define name     fdopencv
%define distname FDOpenCV
%define ver      0.0.1
%define rel      1

Summary: OpenCV Toolkit for FlowDesigner
Name: %name
Version: %ver
Release: %rel
License: GPL
Group: Application/Devel
Source: http://download.sourceforge.net/flowdesigner/%{distname}-%{ver}.tar.gz
URL: http://manyears.sourceforge.net/
Vendor: Laborius
Packager: Dominic Letourneau (dominic.letourneau@usherbrooke.ca)

BuildRoot: %{_tmppath}/root-%{name}-%{version}

%description
Open CV plugin for FlowDesigner

%prep
%setup -n %distname-%ver

%build
export CXXFLAGS='-O3 -msse'
export CFLAGS='-O3 -msse'
./configure --prefix=/usr
%{__make} -j8

%install
%makeinstall

%post -p /sbin/ldconfig
%postun -p /sbin/ldconfig

%files
%defattr(-, root, root, 0755)
/usr/lib/flowdesigner/toolbox/FDOpenCV
/usr/share/flowdesigner/toolbox/FDOpenCV
