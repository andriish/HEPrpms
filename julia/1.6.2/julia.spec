%global uvcommit fb3e3364c33ae48c827f6b103e05c3f0e78b79a9
%global uvversion 2.0.0

%global llvmversion 11.0.1

# Bundled for EPEL7 due to https://src.fedoraproject.org/rpms/suitesparse/pull-request/2
%global suitesparseversion 5.4.0

%global libwhichcommit 81e9723c0273d78493dc8c8ed570f68d9ce7e89e

%global pkgcommit 5c9d3a82e363d98db7f8e99c92203774878b3df0
%global statisticscommit 4b3ef9aaa79350510ca0be395458f66051c2f92d

%global logocommit 168fb6c1164e341df360ed6ced519e1e0cb7de3a

%global __provides_exclude_from ^%{_libdir}/%{name}/.*\\.so.*$
# List all bundled libraries here
# OpenBLAS is excluded because we set a symlink to libopenblasp
%if 0%{?el7}
%global _privatelibs lib(julia-internal|openblas_|openblas64_|suitesparse_wrapper|mpfr|ccalltest|LLVM-.*|uv|unwind|spqr|umfpack|colamd|cholmod|ccolamd|camd|amd|suitesparseconfig|git2|mbed.*)\\.so.*
%else
%global _privatelibs lib(julia-internal|openblas_|openblas64_|suitesparse_wrapper|ccalltest|LLVM-.*|uv)\\.so.*
%endif
%global __provides_exclude ^(%{_privatelibs})$
%global __requires_exclude ^(%{_privatelibs})$

Name:           julia
Version:        1.6.2
Release:        1%{?dist}
Summary:        High-level, high-performance dynamic language for technical computing
# Julia itself is MIT, with a few LGPLv2+ and GPLv2+ files
# libuv is MIT
License:        MIT and LGPLv2+ and GPLv2+
URL:            http://julialang.org/
Source0:        https://github.com/JuliaLang/julia/releases/download/v%{version}/julia-%{version}.tar.gz
# Julia currently uses a custom version of libuv, patches are not yet upstream
Source1:        https://api.github.com/repos/JuliaLang/libuv/tarball/%{uvcommit}#/libuv-%{uvcommit}.tar.gz
Source2:        https://github.com/llvm/llvm-project/releases/download/llvmorg-%{llvmversion}/llvm-%{llvmversion}.src.tar.xz
Source3:        https://api.github.com/repos/vtjnash/libwhich/tarball/%{libwhichcommit}#/libwhich-%{libwhichcommit}.tar.gz
Source4:        https://api.github.com/repos/JuliaLang/Pkg.jl/tarball/%{pkgcommit}#/Pkg-%{pkgcommit}.tar.gz
Source5:        https://api.github.com/repos/JuliaLang/Statistics.jl/tarball/%{statisticscommit}#/Statistics-%{statisticscommit}.tar.gz
Source6:        https://raw.githubusercontent.com/JuliaLang/julia-logo-graphics/%{logocommit}/images/julia-logo-color.svg
Provides:       bundled(libuv) = %{uvversion}
Provides:       bundled(llvm) = %{llvmversion}
BuildRequires:  ca-certificates
BuildRequires:  desktop-file-utils
BuildRequires:  dSFMT-devel
%if 0%{?el7}
BuildRequires:  devtoolset-9
BuildRequires:  devtoolset-9-gcc
BuildRequires:  devtoolset-9-gcc-gfortran
BuildRequires:  devtoolset-9-gcc-c++
BuildRequires:  autoconf
%else
BuildRequires:  gcc
BuildRequires:  gcc-gfortran
BuildRequires:  gcc-c++
%endif
BuildRequires:  gmp-devel >= 5.0
# Needed for libgit2 test
BuildRequires:  hostname
BuildRequires:  ImageMagick
BuildRequires:  libatomic
BuildRequires:  libunwind-devel >= 1.3
BuildRequires:  openblas-devel
BuildRequires:  openblas-threads
BuildRequires:  openlibm-devel >= 0.4
BuildRequires:  libgit2-devel
# Needed for libgit2 test
BuildRequires:  openssl
BuildRequires:  mbedtls-devel
BuildRequires:  libssh2-devel
BuildRequires:  http-parser-devel
BuildRequires:  openssl-devel
BuildRequires:  libcurl-devel
BuildRequires:  libnghttp2-devel
BuildRequires:  curl
BuildRequires:  pcre2-devel
%if 0%{?el7}
BuildRequires:  cmake3
%else
BuildRequires:  cmake
%endif
BuildRequires:  make

%if %{?rhel}%{!?rhel:0}
BuildRequires: python3 python3-devel
%else
BuildRequires:  mpfr-devel >= 4
%endif
BuildRequires:  patchelf
BuildRequires:  perl
BuildRequires:  p7zip-plugins
%if 0%{?el7}
%else
%if 0%{?__isa_bits} == 64
BuildRequires:  suitesparse64_-devel >= 4.1
%else
BuildRequires:  suitesparse-devel >= 4.1
%endif
%endif
BuildRequires:  utf8proc-devel >= 2.1
BuildRequires:  zlib-devel
Requires:       julia-common = %{version}-%{release}
Requires:       ca-certificates
Requires:       p7zip-plugins
%if 0%{?el7}
%else
# Libraries used by CompilerSupportLibraries_jll
# but not detected as they are dlopen()ed but not linked to
%if 0%{?__isa_bits} == 64
Requires:       libgfortran.so.5()(64bit)
Requires:       libgomp.so.1()(64bit)
%else
Requires:       libgfortran.so.5
Requires:       libgomp.so.1
%endif
%endif
# https://bugzilla.redhat.com/show_bug.cgi?id=1158026
# https://github.com/JuliaLang/julia/issues/30087
ExcludeArch:    s390x ppc64le %{arm} aarch64

%description
Julia is a high-level, high-performance dynamic programming language
for technical computing, with syntax that is familiar to users of
other technical computing environments. It provides a sophisticated
compiler, distributed parallel execution, numerical accuracy, and an
extensive mathematical function library. The library, largely written
in Julia itself, also integrates mature, best-of-breed C and Fortran
libraries for linear algebra, random number generation, signal processing,
and string processing.

This package only contains the essential parts of the Julia environment:
the julia executable and the standard library.

%package common
Summary:        Julia architecture-independent files
BuildArch:      noarch
Requires:       julia = %{version}-%{release}

%description common
Contains architecture-independent files required to run Julia.

%package doc
Summary:        Julia documentation and code examples
BuildArch:      noarch
Requires:       julia = %{version}-%{release}

%description doc
Contains the Julia manual, the reference documentation of the standard library
and code examples.

%package devel
Summary:        Julia development, debugging and testing files
Requires:       julia%{?_isa} = %{version}-%{release}

%description devel
Contains library symbolic links and header files for developing applications
linking to the Julia library, in particular embedding it, as well as
tests. This package is normally not
needed when programming in the Julia language, but rather for embedding
Julia into external programs or debugging Julia itself.

%prep
%setup -q

mkdir -p deps/srccache stdlib/srccache

pushd deps/srccache
    # Julia downloads tarballs for external dependencies even when the folder is present:
    # we need to copy the tarball and let the build process unpack it
    # https://github.com/JuliaLang/julia/pull/10280
    cp -p %SOURCE1 .
    cp -p %SOURCE2 .
    cp -p %SOURCE3 .
popd

pushd stdlib/srccache
    cp -p %SOURCE4 .
    cp -p %SOURCE5 .
popd

cp -p %SOURCE6 contrib/julia.svg

# Required so that the image is not optimized for the build CPU
# (i386 does not work yet: https://github.com/JuliaLang/julia/issues/7185)
# Without specifying MARCH, the Julia system image would only work on native CPU
# CPU targets reflect those used upstream at
# https://github.com/JuliaCI/julia-buildbot/blob/master/master/inventory.py
%ifarch %{ix86}
%global march MARCH=pentium4
%global cpu_target JULIA_CPU_TARGET="pentium4;sandybridge,-xsaveopt,clone_all"
%endif
%ifarch x86_64
%global march MARCH=x86-64
%global cpu_target JULIA_CPU_TARGET="generic;sandybridge,-xsaveopt,clone_all;haswell,-rdrnd,base(1)"
%endif
%ifarch %{arm}
# gcc and LLVM do not support the same targets
%global march MARCH=$(echo %optflags | grep -Po 'march=\\K[^ ]*')
%global cpu_target JULIA_CPU_TARGET="generic"
%endif
%ifarch armv7hl
%global march MARCH=$(echo %optflags | grep -Po 'march=\\K[^ ]*')
%global cpu_target JULIA_CPU_TARGET="armv7-a;armv7-a,neon;armv7-a,neon,vfp4"
%endif
%ifarch aarch64
%global march MARCH=armv8-a
%global cpu_target JULIA_CPU_TARGET="generic"
%endif
%ifarch ppc64le
%global march %{nil}
%global cpu_target JULIA_CPU_TARGET="pwr8"
%endif

# Use the non-threaded OpenBLAS library name internally to match what Julia uses so that
# libraries built using BinaryBuilder (like Arpack.jl) work
# We symlink it to libopenblasp below so that threads are used in the end
%if 0%{?__isa_bits} == 64
%global blas USE_BLAS64=1 OPENBLAS_SYMBOLSUFFIX=64_ LIBBLAS=-lopenblas64_ LIBBLASNAME=libopenblas64_ LIBLAPACK=-lopenblas64_ LIBLAPACKNAME=libopenblas64_
%else
%global blas LIBBLAS=-lopenblas LIBBLASNAME=libopenblas LIBLAPACK=-lopenblas LIBLAPACKNAME=libopenblas
%endif

%if 0%{?el7}
%global suitesparse_lib %{nil}
%else
%if 0%{?__isa_bits} == 64
%global suitesparse_lib SUITESPARSE_LIB="-lumfpack64_ -lcholmod64_ -lamd64_ -lcamd64_ -lcolamd64_ -lspqr64_"
%else
%global suitesparse_lib SUITESPARSE_LIB="-lumfpack -lcholmod -lamd -lcamd -lcolamd -lspqr"
%endif
%endif

%if 0%{?el7}
%global cmake CMAKE=cmake3
%else
%global cmake CMAKE=cmake
%endif

# About build, build_libdir and build_bindir, see https://github.com/JuliaLang/julia/issues/5063#issuecomment-32628111
%global commonopts USE_SYSTEM_LLVM=0 USE_SYSTEM_LIBUNWIND=%{?el7:0}%{!?el7:1} USE_SYSTEM_PCRE=%{?el7:0}%{!?el7:1} USE_SYSTEM_BLAS=1 USE_SYSTEM_LAPACK=1 USE_SYSTEM_GMP=1 USE_SYSTEM_MPFR=%{?el7:0}%{!?el7:1} USE_SYSTEM_SUITESPARSE=%{?el7:0}%{!?el7:1} USE_SYSTEM_DSFMT=1 USE_SYSTEM_LIBUV=0 USE_SYSTEM_UTF8PROC=%{?el7:0}%{!?el7:1} USE_SYSTEM_LIBGIT2=%{?el7:0}%{!?el7:1} USE_SYSTEM_LIBSSH2=1 USE_SYSTEM_MBEDTLS=%{?el7:0}%{!?el7:1} USE_SYSTEM_CURL=1 USE_SYSTEM_PATCHELF=1 USE_SYSTEM_LIBM=0 USE_SYSTEM_OPENLIBM=1 USE_SYSTEM_ZLIB=1 USE_SYSTEM_P7ZIP=1 USE_SYSTEM_NGHTTP2=1 USE_SYSTEM_CSL=%{?el7:0}%{!?el7:1} USE_BINARYBUILDER=0 BUNDLE_DEBUG_LIBS=0 JULIA_SPLITDEBUG=1 TAGGED_RELEASE_BANNER="nalimilan/julia Copr build" VERBOSE=1 %{cmake} %{march} %{cpu_target} %{blas} %{suitesparse_lib} prefix=%{_prefix} bindir=%{_bindir} libdir=%{_libdir} libexecdir=%{_libexecdir} datarootdir=%{_datarootdir} includedir=%{_includedir} sysconfdir=%{_sysconfdir} build_prefix=%{_builddir}/%{buildsubdir}/build%{_prefix} JULIA_CPU_THREADS=$(echo %{?_smp_mflags} | sed s/-j//)

%build
# Workaround for https://github.com/JuliaLang/julia/issues/27118
%global optflags %(echo %{optflags} | sed 's/-Wp,-D_GLIBCXX_ASSERTIONS //')
# Workaround for https://github.com/JuliaLang/julia/issues/39822
# and https://bugzilla.redhat.com/show_bug.cgi?id=1928696
%global optflags %(echo %{optflags} | sed 's/-Wp,-D_GNU_SOURCE //')

# Julia hardcodes the exact SOVERSION it uses when USE_SYSTEM_*=0
# https://github.com/JuliaLang/julia/pull/38347#discussion_r574819534
sed "s/libmbedtls.so.*\"/$(cd %{_libdir} && ls libmbedtls.so.??)\"/" -i stdlib/MbedTLS_jll/src/MbedTLS_jll.jl
sed "s/libmbedcrypto.so.*\"/$(cd %{_libdir} && ls libmbedcrypto.so.?)\"/" -i stdlib/MbedTLS_jll/src/MbedTLS_jll.jl
sed "s/libmbedx509.so.*\"/$(cd %{_libdir} && ls libmbedx509.so.?)\"/" -i stdlib/MbedTLS_jll/src/MbedTLS_jll.jl
sed "s/libopenlibm.so.*\"/$(cd %{_libdir} && ls libopenlibm.so.?)\"/" -i stdlib/OpenLibm_jll/src/OpenLibm_jll.jl
sed "s/libgit2.so.*\"/$(cd %{_libdir} && ls libgit2.so.?.?)\"/" -i stdlib/LibGit2_jll/src/LibGit2_jll.jl

# Work around build failure with glibc 2.33 when GNU_SOURCE is set
# https://github.com/JuliaLang/julia/issues/39822
sed "s/#if defined(MINSIGSTKSZ) && MINSIGSTKSZ > 131072/#if 0/" -i src/task.c

# Decrease debuginfo verbosity to reduce memory consumption during final library linking
%ifarch %{arm} %{ix86}
%global optflags %(echo %{optflags} | sed 's/-g /-g1 /')
%endif

%ifarch %{ix86}
# Need to repeat -march here to override i686 from optflags
%global buildflags CFLAGS="%optflags -march=pentium4" CXXFLAGS="%optflags -march=pentium4"
%else
%global buildflags CFLAGS="%optflags" CXXFLAGS="%optflags"
%endif

%if 0%{?el7}
# Required to work around a bug when building libunwind on RHEL7:
# https://github.com/JuliaLang/julia/issues/15496
echo "MAKEOVERRIDES =" >> deps/Makefile
. /opt/rh/devtoolset-9/enable
%else
# Needed when USE_SYSTEM_CSL=1
# https://github.com/JuliaLang/julia/issues/39637
mkdir -p %{_builddir}/%{buildsubdir}/build/usr/lib/
ln -sf %{_libdir}/libgcc_s.so.1 %{_builddir}/%{buildsubdir}/build/usr/lib/libgcc_s.so.1
%endif

make %{?_smp_mflags} %{buildflags} %{commonopts} release

%install
%if 0%{?el7}
. /opt/rh/devtoolset-9/enable
%endif

make %{?_smp_mflags} %{buildflags} %{commonopts} DESTDIR=%{buildroot} install

pushd %{buildroot}%{_libdir}/julia
    %if 0%{?__isa_bits} == 64
        rm -f libopenblas64_.so
        ln -s %{_libdir}/libopenblasp64_.so.0 libopenblas64_.so
        ln -s %{_libdir}/libopenblasp64_.so.0 libopenblas64_.so.0
        # Raise an error in case of failure
        realpath -e libopenblas64_.so
        realpath -e libopenblas64_.so.0

        %if !0%{?el7}
            # Julia creates symlinks to SuiteSparse libraries linking to libopenblas rather than libopenblas64_
            for LIB in spqr umfpack colamd cholmod ccolamd camd amd suitesparseconfig btf klu ldl rbio
            do
                rm -f lib${LIB}.so
                ln -s %{_libdir}/$(readelf -d %{_libdir}/lib${LIB}64_.so | sed -n '/SONAME/s/.*\(lib[^ ]*\.so\.[0-9]*\).*/\1/p') lib${LIB}.so
                # Raise an error in case of failure
                realpath -e lib${LIB}.so
            done
        %endif
    %else
        rm -f libopenblas.so
        ln -s %{_libdir}/libopenblasp.so.0 libopenblas.so
        ln -s %{_libdir}/libopenblasp.so.0 libopenblas.so.0
        # Raise an error in case of failure
        realpath -e libopenblas.so
        realpath -e libopenblas.so.0
    %endif
popd

cp -p CONTRIBUTING.md LICENSE.md NEWS.md README.md %{buildroot}%{_docdir}/julia/

pushd %{buildroot}%{_libdir}/julia
    # Some Julia packages rely on being able to use libjulia, but we only
    # ship %%{_libdir}/libjulia.so in the -devel package
    ln -s ../libjulia.so.1 libjulia.so
    # Raise an error in case of failure
    realpath -e libjulia.so

    # Needed when USE_SYSTEM_CSL=1
    # https://github.com/JuliaLang/julia/issues/39637
    ln -sf %{_libdir}/libgcc_s.so.1 libgcc_s.so.1
    # Raise an error in case of failure
    realpath -e libgcc_s.so.1

    %if 0%{?el7}
        ln -sf %{_libdir}/libpthread.so.0 libpthread.so.0
        # Raise an error in case of failure
        realpath -e libpthread.so.0
    %endif
popd

# Use CA certificates from ca-certificates
# (Mozilla certificates are not installed anyway when USE_SYSTEM_LIBGIT2=1)
# https://github.com/JuliaLang/julia/commit/5dc6201e8dccbf21aeeb1f79fef2d186c7800a4e#r47032178
ln -sf /etc/pki/tls/cert.pem %{buildroot}%{_datarootdir}/julia/cert.pem

# Install .desktop file and icons
mkdir -p %{buildroot}%{_datarootdir}/icons/hicolor/scalable/apps/
mkdir -p %{buildroot}%{_datarootdir}/icons/hicolor/16x16/apps/
mkdir -p %{buildroot}%{_datarootdir}/icons/hicolor/24x24/apps/
mkdir -p %{buildroot}%{_datarootdir}/icons/hicolor/32x32/apps/
mkdir -p %{buildroot}%{_datarootdir}/icons/hicolor/48x48/apps/
mkdir -p %{buildroot}%{_datarootdir}/icons/hicolor/256x256/apps/
cp -p contrib/julia.svg %{buildroot}%{_datarootdir}/icons/hicolor/scalable/apps/%{name}.svg
convert -scale 16x16 -extent 16x16 -gravity center -background transparent \
    contrib/julia.svg %{buildroot}%{_datarootdir}/icons/hicolor/16x16/apps/%{name}.png
convert -scale 24x24 -extent 24x24 -gravity center -background transparent \
    contrib/julia.svg %{buildroot}%{_datarootdir}/icons/hicolor/24x24/apps/%{name}.png
convert -scale 32x32 -extent 32x32 -gravity center -background transparent \
    contrib/julia.svg %{buildroot}%{_datarootdir}/icons/hicolor/32x32/apps/%{name}.png
convert -scale 48x48 -extent 48x48 -gravity center -background transparent \
    contrib/julia.svg %{buildroot}%{_datarootdir}/icons/hicolor/48x48/apps/%{name}.png
convert -scale 256x256 -extent 256x256 -gravity center -background transparent \
    contrib/julia.svg %{buildroot}%{_datarootdir}/icons/hicolor/256x256/apps/%{name}.png
desktop-file-validate %{buildroot}%{_datarootdir}/applications/%{name}.desktop

%files
%dir %{_docdir}/julia/
%{_docdir}/julia/LICENSE.md
%doc %{_docdir}/julia/CONTRIBUTING.md
%doc %{_docdir}/julia/NEWS.md
%doc %{_docdir}/julia/README.md
%{_bindir}/julia
%{_libdir}/julia/
%exclude %{_libdir}/julia/*debug*
%{_libdir}/libjulia.so.*
%{_mandir}/man1/julia.1*
%{_datarootdir}/appdata/julia.appdata.xml
%{_datarootdir}/applications/%{name}.desktop
%{_datarootdir}/icons/hicolor/scalable/apps/%{name}.svg
%{_datarootdir}/icons/hicolor/16x16/apps/%{name}.png
%{_datarootdir}/icons/hicolor/24x24/apps/%{name}.png
%{_datarootdir}/icons/hicolor/32x32/apps/%{name}.png
%{_datarootdir}/icons/hicolor/48x48/apps/%{name}.png
%{_datarootdir}/icons/hicolor/256x256/apps/%{name}.png

%files common
%dir %{_datarootdir}/julia/
%{_datarootdir}/julia/*.jl
%{_datarootdir}/julia/base/
%{_datarootdir}/julia/stdlib/
%{_datarootdir}/julia/base.cache
%{_datarootdir}/julia/cert.pem

%dir %{_sysconfdir}/julia/
%config(noreplace) %{_sysconfdir}/julia/startup.jl

%files doc
%doc %{_docdir}/julia/

%files devel
%{_libdir}/libjulia.so
%{_libdir}/julia/libccalltest.so.debug
%{_includedir}/julia/
%{_datarootdir}/julia/test/

%post
/sbin/ldconfig
/bin/touch --no-create %{_datarootdir}/icons/hicolor &>/dev/null || :
exit 0

%changelog
* Thu Aug 12 2021 Milan Bouchet-Valat <nalimilan@club.fr> - 1.6.2-1
- New upstream release.

* Sat Apr 24 2021 Milan Bouchet-Valat <nalimilan@club.fr> - 1.6.1-1
- New upstream release.

* Thu Mar 25 2021 Milan Bouchet-Valat <nalimilan@club.fr> - 1.6.0-1
- New upstream release.

* Fri Mar 12 2021 Milan Bouchet-Valat <nalimilan@club.fr> - 1.6.0-0.5.rc2
- New upstream release.
- Really fix build on Rawhide.

* Thu Feb 25 2021 Milan Bouchet-Valat <nalimilan@club.fr> - 1.6.0-0.4.rc1
- Fix build on Rawhide.

* Tue Feb 16 2021 Milan Bouchet-Valat <nalimilan@club.fr> - 1.6.0-0.3.rc1
- Fix libgfortran.so dependency on 64-bit.

* Mon Feb 15 2021 Milan Bouchet-Valat <nalimilan@club.fr> - 1.6.0-0.2.rc1
- Fix libgfortran.so version.

* Sun Feb 14 2021 Milan Bouchet-Valat <nalimilan@club.fr> - 1.6.0-0.1.rc1
- New upstream release.

* Tue Jan 26 2021 Fedora Release Engineering <releng@fedoraproject.org> - 1.5.3-4
- Rebuilt for https://fedoraproject.org/wiki/Fedora_34_Mass_Rebuild

* Mon Jan 4 2021 Milan Bouchet-Valat <nalimilan@club.fr> - 1.5.3-3
- Fix build failure.

* Mon Dec 28 2020 Igor Raits <ignatenkobrain@fedoraproject.org> - 1.5.3-2
- Rebuild for libgit2 1.1.x

* Wed Nov 11 2020 Milan Bouchet-Valat <nalimilan@club.fr> - 1.5.3-1
- New upstream release.

* Sun Sep 27 2020 Milan Bouchet-Valat <nalimilan@club.fr> - 1.5.2-1
- New upstream release.

* Mon Aug 10 2020 Milan Bouchet-Valat <nalimilan@club.fr> - 1.5.0-1
- New upstream release.
- No longer include julia-debug to work around build failure (rhbz#1863925).

* Sat Aug 01 2020 Fedora Release Engineering <releng@fedoraproject.org> - 1.4.2-4
- Second attempt - Rebuilt for
  https://fedoraproject.org/wiki/Fedora_33_Mass_Rebuild

* Tue Jul 28 2020 Fedora Release Engineering <releng@fedoraproject.org> - 1.4.2-3
- Rebuilt for https://fedoraproject.org/wiki/Fedora_33_Mass_Rebuild

* Sun Jun 14 2020 Milan Bouchet-Valat <nalimilan@club.fr> - 1.4.2-2
- Fix error on startup due to incorrect libLLVM name.

* Sat May 30 2020 Milan Bouchet-Valat <nalimilan@club.fr> - 1.4.2-1
- New upstream release.

* Sun Apr 19 2020 Igor Raits <ignatenkobrain@fedoraproject.org> - 1.4.0-3
- Rebuild for libgit2 1.0.0

* Fri Mar 27 2020 Milan Bouchet-Valat <nalimilan@club.fr> - 1.4.0-2
- Fix error on startup.

* Tue Mar 24 2020 Milan Bouchet-Valat <nalimilan@club.fr> - 1.4.0-1
- New upstream release.

* Wed Jan 29 2020 Fedora Release Engineering <releng@fedoraproject.org> - 1.2.0-6
- Rebuilt for https://fedoraproject.org/wiki/Fedora_32_Mass_Rebuild

* Tue Nov 5 2019 Milan Bouchet-Valat <nalimilan@club.fr> - 1.2.0-5
- Include libjulia.so symlink in private Julia libdir so that packages
  can call into libjulia even when julia-devel is not installed (fixes rhbz#1764797).

* Mon Oct 21 2019 Milan Bouchet-Valat <nalimilan@club.fr> - 1.2.0-4
- Unbundle SuiteSparse, mpfr and libunwind.

* Tue Oct 8 2019 Milan Bouchet-Valat <nalimilan@club.fr> - 1.2.0-3
- Fix missing libopenblas64_.so.0 symlink to fix rhbz#1758803.

* Tue Aug 27 2019 Milan Bouchet-Valat <nalimilan@club.fr> - 1.2.0-2
- Unbundle PCRE.

* Sun Aug 25 2019 Milan Bouchet-Valat <nalimilan@club.fr> - 1.2.0-1
- New upstream release.
- Use openblas(64_).so as internal library name to fix packages like Arpack.jl.
- Bundle PCRE to work around rhbz#1743863.
- Move libccalltest.so.debug and sys-debug.so to julia-devel.
- Disable ARM architectures for now due to test failures.

* Thu Jul 25 2019 Fedora Release Engineering <releng@fedoraproject.org> - 1.1.0-4
- Rebuilt for https://fedoraproject.org/wiki/Fedora_31_Mass_Rebuild

* Thu Jun 06 2019 Igor Gnatenko <ignatenkobrain@fedoraproject.org> - 1.1.0-3
- Rebuild for libgit2 0.28.x

* Fri Feb 01 2019 Fedora Release Engineering <releng@fedoraproject.org> - 1.1.0-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_30_Mass_Rebuild

* Tue Jan 22 2019 Milan Bouchet-Valat <nalimilan@club.fr> - 1.1.0-1
- New upstream release.

* Wed Dec 19 2018 Milan Bouchet-Valat <nalimilan@club.fr> - 1.0.3-1
- New upstream release.

* Sat Nov 10 2018 Milan Bouchet-Valat <nalimilan@club.fr> - 1.0.2-1
- New upstream release.

* Mon Oct 29 2018 Milan Bouchet-Valat <nalimilan@club.fr> - 1.0.1-5
- Drop unnecessary dependency of julia on julia-devel and openblas-threads.
- Add Fedora to release banner.

* Sat Oct 20 2018 Milan Bouchet-Valat <nalimilan@club.fr> - 1.0.1-4
- Use ILP64 BLAS and bundle SuiteSparse until system packages support it.

* Sat Oct 06 2018 Morten Stevens <mstevens@fedoraproject.org> - 1.0.1-3
- Rebuilt for mbed TLS 2.13.0

* Wed Oct 3 2018 Milan Bouchet-Valat <nalimilan@club.fr> - 1.0.1-2
- Make package installable again by fixing Requires.

* Sun Sep 30 2018 Milan Bouchet-Valat <nalimilan@club.fr> - 1.0.1-1
- New upstream release.
- Remove internal libraries from Provides.
- Enable build on ARM and PPC.

* Fri Sep 7 2018 Milan Bouchet-Valat <nalimilan@club.fr> - 1.0.0-2
- Fix FTBFS by bundling libunwind.

* Fri Sep 7 2018 Milan Bouchet-Valat <nalimilan@club.fr> - 1.0.0-1
- New upstream release 1.0.0.

* Fri Aug 10 2018 Igor Gnatenko <ignatenkobrain@fedoraproject.org> - 0.6.3-3
- Rebuild for libgit2 0.27.x

* Fri Jul 13 2018 Fedora Release Engineering <releng@fedoraproject.org> - 0.6.3-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_29_Mass_Rebuild

* Sun Jun 3 2018 Milan Bouchet-Valat <nalimilan@club.fr> - 0.6.3-1
- New upstream release.

* Fri Mar 23 2018 Milan Bouchet-Valat <nalimilan@club.fr> - 0.6.2-3
- Work around bug in UNW_VERSION_MINOR not being a single integer by removing redundant check.
- Fix libgit2 test failure due to letter case.

* Wed Feb 07 2018 Fedora Release Engineering <releng@fedoraproject.org> - 0.6.2-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_28_Mass_Rebuild

* Thu Dec 14 2017 Milan Bouchet-Valat <nalimilan@club.fr> - 0.6.2-1
- New upstream release.

* Thu Aug 03 2017 Fedora Release Engineering <releng@fedoraproject.org> - 0.6.0-4
- Rebuilt for https://fedoraproject.org/wiki/Fedora_27_Binutils_Mass_Rebuild

* Wed Jul 26 2017 Fedora Release Engineering <releng@fedoraproject.org> - 0.6.0-3
- Rebuilt for https://fedoraproject.org/wiki/Fedora_27_Mass_Rebuild

* Sun Jul 09 2017 Milan Bouchet-Valat <nalimilan@club.fr> - 0.6.0-2
- Fix build with libgit2 0.26.

* Sat Jul 08 2017 Igor Gnatenko <ignatenko@redhat.com> - 0.6.0-2
- Rebuild for libgit2 0.26.x

* Thu Jun 22 2017 Milan Bouchet-Valat <nalimilan@club.fr> - 0.6.0-1
- New upstream release.

* Sun Jun 11 2017 Milan Bouchet-Valat <nalimilan@club.fr> - 0.6.0-0.5.rc3
- New upstream release.
- Use system libunwind instead of bundling it.

* Thu May 25 2017 Peter Robinson <pbrobinson@fedoraproject.org> 0.6.0-0.4.rc2
- Rebuild llvm-4

* Fri May 19 2017 Milan Bouchet-Valat <nalimilan@club.fr> - 0.6.0-0.3.rc2
- New upstream release.

* Sat Apr 1 2017 Milan Bouchet-Valat <nalimilan@club.fr> - 0.6.0-0.2.pre.beta
- New upstream release.

* Thu Mar 2 2017 Milan Bouchet-Valat <nalimilan@club.fr> - 0.6.0-0.1.pre.alpha
- New upstream release, fixes build with libgit 0.25.

* Tue Feb 21 2017 Milan Bouchet-Valat <nalimilan@club.fr> - 0.5.0-3
- Rebuild for GCC7.

* Tue Feb 07 2017 Igor Gnatenko <ignatenko@redhat.com> - 0.5.0-2
- Rebuild for libgit2-0.25.x

* Tue Sep 20 2016 Milan Bouchet-Valat <nalimilan@club.fr> - 0.5.0-1
- New upstream release.

* Thu Sep 15 2016 Milan Bouchet-Valat <nalimilan@club.fr> - 0.5.0-0.rc4
- New upstream release candidate.

* Mon Jun 20 2016 Milan Bouchet-Valat <nalimilan@club.fr> - 0.4.6-1
- New upstream release.
- Drop tridiag patch, now included upstream.

* Wed Mar 30 2016 Igor Gnatenko <i.gnatenko.brain@gmail.com> - 0.4.5-2
- Rebuild for libgit2 0.24.0 once more

* Sun Mar 20 2016 Milan Bouchet-Valat <nalimilan@club.fr> - 0.4.5-1
- New upstream release.

* Sun Mar 20 2016 Milan Bouchet-Valat <nalimilan@club.fr> - 0.4.3-9
- Add patch to fix non-deterministic test failure with OpenBLAS 0.2.16.

* Sun Mar 20 2016 Igor Gnatenko <i.gnatenko.brain@gmail.com> - 0.4.3-8
- Rebuild for libgit2 0.24.0

* Tue Mar 8 2016 Milan Bouchet-Valat <nalimilan@club.fr> - 0.4.3-7
- Fix generation of library symlinks to rely only on major version.
- Rebuild for openlibm SONAME bump.
- Use openlibm on all platforms.

* Wed Mar 2 2016 Milan Bouchet-Valat <nalimilan@club.fr> - 0.4.3-6
- Fix missing PCRE2 dependency, use realpath -e to detect this problem.

* Tue Mar 1 2016 Milan Bouchet-Valat <nalimilan@club.fr> - 0.4.3-5
- Automate generation of library symlinks, and include them in the package instead of
  in %%post so that dependencies on specific library versions are detected.

* Fri Feb 26 2016 Suvayu Ali <fatkasuvayu+linux@gmail.com> - 0.4.3-4
- Fix broken symlinks in libdir

* Thu Feb 04 2016 Fedora Release Engineering <releng@fedoraproject.org> - 0.4.3-3
- Rebuilt for https://fedoraproject.org/wiki/Fedora_24_Mass_Rebuild

* Thu Jan 28 2016 Milan Bouchet-Valat <nalimilan@club.fr> - 0.4.3-2
- Fix build with GCC 6.

* Thu Jan 28 2016 Milan Bouchet-Valat <nalimilan@club.fr> - 0.4.3-1
- New upstream release.
- Revert to LP64 OpenBLAS until ILP64 works correctly.

* Wed Jan 27 2016 Adam Jackson <ajax@redhat.com> 0.4.2-4
- Rebuild for llvm 3.7.1 library split

* Tue Jan 5 2016 Orion Poplawski <orion@cora.nwra.com> - 0.4.2-3
- Use proper conditional for __isa_bits tests

* Thu Dec 24 2015 Milan Bouchet-Valat <nalimilan@club.fr> - 0.4.2-2
- Use new ILP64 OpenBLAS, suffixed with 64_ (ARPACK and SuiteSparse still use
  the LP64 Atlas).

* Wed Dec 9 2015 Milan Bouchet-Valat <nalimilan@club.fr> - 0.4.2-1
- New upstream release.
- Update bundled libuv to latest Julia fork.

* Mon Nov 9 2015 Milan Bouchet-Valat <nalimilan@club.fr> - 0.4.1-1
- New upstream release.
- Pass explicitly -march to override default -march=i686 with pentium4.
- Get rid of useless build dependencies.

* Fri Oct 9 2015 Milan Bouchet-Valat <nalimilan@club.fr> - 0.4.0-2
- Use LLVM 3.3 to fix bugs and improve compilation performance.
- Run all the tests now that they pass.
- Stop specifying -fsigned-char explicitly, since it is now handled by Julia.
- Refactor architecture checking logic to prepare support for new arches.
- Use upstream .desktop file instead of a custom one.

* Fri Oct 9 2015 Milan Bouchet-Valat <nalimilan@club.fr> - 0.4.0-1
- New upstream release.
- Drop patches now included upstream.
- Drop obsolete rm commands.

* Thu Sep 17 2015 Dave Airlie <airlied@redhat.com> 0.4.0-0.4.rc1
- drag in latest upstream 0.4 branch in hope of fixing i686
- drop out some tests on i686
- build against LLVM 3.7

* Fri Sep 11 2015 Milan Bouchet-Valat <nalimilan@club.fr> - 0.4.0-0.3.rc1
- New upstream release candidate.
- Drop now useless patch.
- Remove libccalltest.so file installed under /usr/share/.

* Fri Aug 28 2015 Nils Philippsen <nils@redhat.com> - 0.4.0-0.2.20150823git
- rebuild against suitesparse-4.4.5, to work around
  https://github.com/JuliaLang/julia/issues/12841

* Sun Aug 23 2015 Milan Bouchet-Valat <nalimilan@club.fr> - 0.4.0-0.1.20150823git
- Update to development version 0.4.0 to fix FTBFS.
- Move to PCRE2, libgit2, utf8proc 1.3, and up-to-date libuv fork.
- Preliminary support for ARM.
- patchelf no longer needed when the same paths are passed to 'make' and 'make install'.
- Building Sphynx documentation no longer needed.
- Fix icons to be square.

* Wed Jun 17 2015 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 0.3.7-5
- Rebuilt for https://fedoraproject.org/wiki/Fedora_23_Mass_Rebuild

* Thu Jun 11 2015 Nils Philippsen <nils@redhat.com> - 0.3.7-4
- rebuild for suitesparse-4.4.4

* Fri Apr 10 2015 Milan Bouchet-Valat <nalimilan@club.fr> - 0.3.7-3
- Rebuilt for LLVM 3.6.

* Sat Mar 28 2015 Milan Bouchet-Valat <nalimilan@club.fr> - 0.3.7-2
- Rebuild for utf8proc ABI break.

* Tue Mar 24 2015 Milan Bouchet-Valat <nalimilan@club.fr> - 0.3.7-1
- New upstream release.

* Mon Mar 2 2015 Milan Bouchet-Valat <nalimilan@club.fr> - 0.3.6-2
- Fix loading libcholmod, libfftw3_threads and libumfpack.

* Tue Feb 17 2015 Milan Bouchet-Valat <nalimilan@club.fr> - 0.3.6-1
- New upstream release.

* Fri Jan 9 2015 Milan Bouchet-Valat <nalimilan@club.fr> - 0.3.5-1
- New upstream release.

* Fri Dec 26 2014 Milan Bouchet-Valat <nalimilan@club.fr> - 0.3.4-1
- New upstream release.

* Fri Dec 12 2014 Adam Jackson <ajax@redhat.com> 0.3.3-2
- Rebuild for F21 LLVM 3.5 rebase

* Sun Nov 23 2014 Milan Bouchet-Valat <nalimilan@club.fr> - 0.3.3-1
- New upstream release.
- Bump libuv to follow upstream.

* Wed Nov 05 2014 Adam Jackson <ajax@redhat.com> 0.3.2-4
- Don't BuildRequire: llvm-static

* Tue Oct 28 2014 Milan Bouchet-Valat <nalimilan@club.fr> - 0.3.2-3
- Trigger rebuild to use LLVM 3.5.

* Thu Oct 23 2014 Milan Bouchet-Valat <nalimilan@club.fr> - 0.3.2-2
- New upstream release.

* Sun Oct 12 2014 Milan Bouchet-Valat <nalimilan@club.fr> - 0.3.1-3
- Fix missing symlinks to libarpack, libpcre, libgmp and libmpfr, which could
  prevent Julia from working correcly if the -devel packages were missing.
- Fix invalid hard-coded reference to /usr/lib64.

* Fri Sep 26 2014 Milan Bouchet-Valat <nalimilan@club.fr> - 0.3.1-2
- Add git to dependencies, as it is needed to install packages.

* Mon Sep 22 2014 Milan Bouchet-Valat <nalimilan@club.fr> - 0.3.1-1
- New upstream version.
- Depend on openblas-threads instead of openblas.
- Make source URL automatically depend on version.

* Sat Sep 20 2014 Peter Robinson <pbrobinson@fedoraproject.org> 0.3.0-10
- Add dist tag

* Fri Sep 19 2014 Milan Bouchet-Valat <nalimilan@club.fr> - 0.3.0-9
- Use libopenblasp to enable threading.
- Make julia-common depend on julia.

* Fri Sep 19 2014 Milan Bouchet-Valat <nalimilan@club.fr> - 0.3.0-8
- Use versioned OpenBLAS library.so to work without openblas-devel.
- Use LAPACK from OpenBLAS instead of reference implementation.
- Add .desktop file.
- Remove objects.inv feil from HTML documentation.

* Thu Sep 18 2014 Milan Bouchet-Valat <nalimilan@club.fr> - 0.3.0-7
- Fix double inclusion of HTML documentation.
- Improve working directory logic.

* Thu Sep 18 2014 Milan Bouchet-Valat <nalimilan@club.fr> - 0.3.0-6
- Do not remove _sources directory in HTML documentation.
- Make -doc depend on julia to avoid mismatches.

* Wed Sep 17 2014 Milan Bouchet-Valat <nalimilan@club.fr> - 0.3.0-5
- Revert to installing performance suite (needed to run tests).
- Fix double inclusion of some documentation files.
- Move architecture-independent files to a -common subpackage.
- Install HTML documentation instead of .rst files.
- Fix build and install paths.
- Remove dependencies on dSFMT-devel, openlibm-devel and openlibm-devel,
  replacing them with private symbolic links.
- Stop installing libjulia.so to libdir.

* Mon Sep 15 2014 Milan Bouchet-Valat <nalimilan@club.fr> - 0.3.0-4
- Do not install non-functional performance test suite and Makefiles.
- Install documentation to docdir instead of /usr/share/julia/doc.
- Clarify comment about Julia's license.

* Mon Sep 15 2014 Milan Bouchet-Valat <nalimilan@club.fr> - 0.3.0-3
- Remove -xnolibs argument passed by libuv to dtrace (no longer supported
  by systemtap 2.5).

* Fri Sep 5 2014 Milan Bouchet-Valat <nalimilan@club.fr> - 0.3.0-2
- Claim ownership of Julia directories where needed.
- Move libjulia.so to the base package instead of -devel.

* Thu Aug 28 2014 Milan Bouchet-Valat <nalimilan@club.fr> - 0.3.0-1
- New upstream 0.3 final release.
- Set MARCH=pentium4 for 32-bit builds to work on CPUs older than core2.
- Use llvm package instead of requiring llvm3.3.
- Temporarily disable failing backtrace test.

* Sat Jul 26 2014 Milan Bouchet-Valat <nalimilan@club.fr> - 0.3.0-0.6.rc1
- Add dSFMT-devel to Requires.
- Use versioned tarball names for libuv and Rmath.

* Sun Jul 06 2014 Milan Bouchet-Valat <nalimilan@club.fr> - 0.3.0-0.5.git
- Bump libuv and libRmath, simplify tarball names.

* Sat Jun 28 2014 Milan Bouchet-Valat <nalimilan@club.fr> - 0.3.0-0.4.git
- Use system dSFMT instead of bundling it.

* Thu Jun 12 2014 Milan Bouchet-Valat <nalimilan@club.fr> - 0.3.0-0.3.git
- Use llvm3.3 package when llvm is 3.4 to avoid failures.
- Fixes to support EPEL.

* Sun May 4 2014 Milan Bouchet-Valat <nalimilan@club.fr> - 0.3.0-0.2.git
- Automatically use the installed LLVM version.
- Mark dSFMT as bundled library and store version in a macro.

* Tue Apr 29 2014 Milan Bouchet-Valat <nalimilan@club.fr> - 0.3.0-0.1.git
- New upstream version 0.3.0.
- Switch to LLVM 3.4.
- Drop useless %%exclude.
- Add blank lines between changelog entries.

* Thu Dec 12 2013 Milan Bouchet-Valat <nalimilan@club.fr> - 0.2.0-2
- Make julia a meta-package and move essential parts to julia-base.
- Use %%{ix86} in ExclusiveArch rather than i386.
- Use %%{buildroot}/%%{_prefix}, %%{_sysconfdir}, %%{_libdir} and %%{_mandir}
  instead of hardcoding paths.
- Use glob pattern to match compressed or uncompressed man pages.
- Move %%post and %%postun before %%files.
- Add blank lines between Changelog entries.

* Wed Dec 11 2013 Milan Bouchet-Valat <nalimilan@club.fr> - 0.2.0-1
- Update to upstream version 0.2.0 and use system libraries as much as possible.

* Thu Jun 14 2012 Orion Poplawski <orion@cora.nwra.com> - 0-0.1.giteecafbe656863a6a8ad4969f53eed358ec2e7555
- Initial package
