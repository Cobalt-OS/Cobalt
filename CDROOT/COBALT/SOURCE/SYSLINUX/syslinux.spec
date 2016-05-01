# -*- rpm -*-
%define RPMVERSION 2.10
%define VERSION    2.10
Summary: Kernel loader which uses a FAT or iso9660 filesystem or a PXE network
Name: syslinux
Version: %{RPMVERSION}
Release: 1
License: GPL
Group: Applications/System
Source0: ftp://ftp.kernel.org/pub/linux/utils/boot/syslinux/%{name}-%{VERSION}.tar.gz
ExclusiveArch: i386 x86_64
Packager: H. Peter Anvin <hpa@zytor.com>
Buildroot: %{_tmppath}/%{name}-%{VERSION}-root
BuildPrereq: nasm >= 0.98.35, perl
Autoreq: 0
%ifarch i386
Requires: mtools, libc.so.6
%endif
%ifarch x86_64
Requires: mtools, libc.so.6()(64bit)
%endif

%description
Syslinux is a simple kernel loader. It normally loads the kernel (and an 
optional initrd image) from a FAT filesystem. It can also be used as a
PXE bootloader during network boots (PXELINUX), or for booting from
ISO 9660 CD-ROMs (ISOLINUX).

%prep
%setup -q -n syslinux-%{VERSION}

%build
make clean
make installer

%install
rm -rf %{buildroot}
mkdir -p %{buildroot}%{_bindir}
mkdir -p %{buildroot}%{_libdir}/syslinux
mkdir -p %{buildroot}%{_includedir}
make install install-lib \
	INSTALLROOT=%{buildroot} BINDIR=%{_bindir} \
	LIBDIR=%{_libdir} INCDIR=%{_includedir}
cp mkdiskimage sys2ansi.pl keytab-lilo.pl %{buildroot}%{_libdir}/syslinux

%clean
rm -rf %{buildroot}

%files
%defattr(-,root,root)
%doc NEWS README *.doc memdisk/*.doc COPYING
%doc sample
%{_bindir}/syslinux
%{_bindir}/ppmtolss16
%{_bindir}/lss16toppm
%{_bindir}/gethostip
%{_libdir}/syslinux
%{_libdir}/libsyslinux.*
%{_includedir}/syslinux.h

%post
/sbin/ldconfig

%postun
/sbin/ldconfig

%changelog
* Wed Apr 16 2003 H. Peter Anvin <hpa@zytor.com> 2.04-1
- 2.04 release
- Add support for libsyslinux.so*
- Templatize for inclusion in CVS tree

* Thu Apr 10 2003 H. Peter Anvin <hpa@zytor.com>
- 2.03 release
- Add support for libsyslinux.a
- Add keytab-lilo.pl to the /usr/lib/syslinux directory
- Modernize syntax
- Support building on x86-64

* Thu Feb 13 2003 H. Peter Anvin <hpa@zytor.com>
- 2.02 release; no longer setuid

* Thu Jan 30 2003 H. Peter Anvin <hpa@zytor.com>
- Prepare for 2.01 release; make /usr/bin/syslinux setuid root

* Fri Oct 25 2002 H. Peter Anvin <hpa@zytor.com>
- Upgrade to 2.00.

* Tue Aug 27 2002 H. Peter Anvin <hpa@zytor.com>
- Upgrade to 1.76.

* Fri Jun 14 2002 H. Peter Anvin <hpa@zytor.com>
- Upgrade to 1.75.

* Sat Jun  1 2002 H. Peter Anvin <hpa@zytor.com>
- Upgrade to 1.74.

* Sun May 26 2002 H. Peter Anvin <hpa@zytor.com>
- Upgrade to 1.73.

* Tue Apr 23 2002 H. Peter Anvin <hpa@zytor.com>
- Upgrade to 1.72.

* Wed Apr 17 2002 H. Peter Anvin <hpa@zytor.com>
- Upgrade to 1.71.
- Update the title.

* Wed Apr 17 2002 H. Peter Anvin <hpa@zytor.com>
- Upgrade to 1.70.

* Sat Feb  3 2002 H. Peter Anvin <hpa@zytor.com>
- Upgrade to 1.67.

* Tue Jan  1 2002 H. Peter Anvin <hpa@zytor.com>
- Upgrade to 1.66.

* Sat Dec 15 2001 H. Peter Anvin <hpa@zytor.com>
- Upgrade to 1.65; make appropriate changes.

* Sat Aug 24 2001 H. Peter Anvin <hpa@zytor.com>
- Upgrade to 1.64.

* Mon Aug  6 2001 H. Peter Anvin <hpa@zytor.com>
- Upgrade to 1.63.
- Use make install since the stock SYSLINUX distribution now supports
  INSTALLROOT.

* Sat Apr 24 2001 H. Peter Anvin <hpa@zytor.com>
- Upgrade to 1.62.

* Sat Apr 14 2001 H. Peter Anvin <hpa@zytor.com>
- Fix missing %files; correct modes.

* Fri Apr 13 2001 H. Peter Anvin <hpa@zytor.com>
- Upgrade to 1.61
- Install auxilliary programs in /usr/lib/syslinux

* Sat Feb 10 2001 Matt Wilson <msw@redhat.com>
- 1.52

* Wed Jan 24 2001 Matt Wilson <msw@redhat.com>
- 1.51pre7

* Mon Jan 22 2001 Matt Wilson <msw@redhat.com>
- 1.51pre5

* Fri Jan 19 2001 Matt Wilson <msw@redhat.com>
- 1.51pre3, with e820 detection

* Tue Dec 12 2000 Than Ngo <than@redhat.com>
- rebuilt with fixed fileutils

* Thu Nov 9 2000 Than Ngo <than@redhat.com>
- update to 1.49
- update ftp site
- clean up specfile
- add some useful documents

* Tue Jul 18 2000 Nalin Dahyabhai <nalin@redhat.com>
- add %%defattr (release 4)

* Wed Jul 12 2000 Prospector <bugzilla@redhat.com>
- automatic rebuild

* Thu Jul 06 2000 Trond Eivind Glomsrød <teg@redhat.com>
- use %%{_tmppath}
- change application group (Applications/Internet doesn't seem
  right to me)
- added BuildRequires

* Tue Apr 04 2000 Erik Troan <ewt@redhat.com>
- initial packaging
