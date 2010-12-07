# _revision, _release, and _version should be defined on the rpmbuild command
# line like so:
#
# --define "_version 0.9.1" --define "_release 7" \
# --define "_revision 0.9.1-19-abcdef"

Name: riak
Version: %{_version}
Release: %{_release}%{?dist}
License: Apache License
Group: Development/Libraries
Source: http://bitbucket.org/basho/riak/get/%{name}-%{_revision}.tar.gz
Source1: riak_init
URL: http://basho.com
Vendor: Basho Technologies
Packager: Basho Technologies <riak-user@lists.basho.com>
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
Summary: Riak Distributed Data Store

%description
Riak is a distrubuted data store.

%define riak_lib %{_libdir}/%{name}
%define init_script %{_sysconfdir}/init.d/%{name}

%define __prelink_undo_cmd /bin/cat prelink library

%prep
%setup -q -n %{name}-%{_revision}
cat > rel/vars.config <<EOF
% app.config
{ring_state_dir, "%{_localstatedir}/lib/%{name}/ring"}.
{web_ip,       "127.0.0.1"}.
{web_port,     8098}.
{handoff_port, 8099}.
{pb_ip,        "127.0.0.1"}.
{pb_port,      8087}.
{bitcask_data_root, "%{_localstatedir}/lib/%{name}/bitcask"}.
{sasl_error_log, "%{_localstatedir}/log/%{name}/sasl-error.log"}.
{sasl_log_dir, "%{_localstatedir}/log/%{name}/sasl"}.
{mapred_queue_dir, "%{_localstatedir}/lib/%{name}/mr_queue"}.
{map_js_vms,   8}.
{reduce_js_vms, 6}.
{hook_js_vms, 2}.
% vm.args
{node,         "riak@127.0.0.1"}.
% bin/riak*
{runner_script_dir,  "/usr/sbin"}.
{runner_base_dir,    "%{riak_lib}"}.
{runner_etc_dir,     "%{_sysconfdir}/%{name}"}.
{runner_log_dir,     "%{_localstatedir}/log/%{name}"}.
{pipe_dir,           "%{_localstatedir}/run/%{name}/"}.
{runner_user,        "%{name}"}.
EOF

%build
mkdir %{name}
make rel

%install
mkdir -p %{buildroot}%{_sysconfdir}/%{name}
mkdir -p %{buildroot}%{riak_lib}
mkdir -p %{buildroot}%{_mandir}/man1
mkdir -p %{buildroot}%{_localstatedir}/lib/%{name}/dets
mkdir -p %{buildroot}%{_localstatedir}/lib/%{name}/bitcask
mkdir -p %{buildroot}%{_localstatedir}/lib/%{name}/ring
mkdir -p %{buildroot}%{_localstatedir}/log/%{name}
mkdir -p %{buildroot}%{_localstatedir}/log/%{name}/sasl
mkdir -p %{buildroot}%{_localstatedir}/run/%{name}
mkdir -p %{buildroot}%{_localstatedir}/lib/%{name}/mr_queue

#Copy all necessary lib files etc.
cp -r $RPM_BUILD_DIR/%{name}-%{_revision}/rel/%{name}/lib %{buildroot}%{riak_lib}
cp -r $RPM_BUILD_DIR/%{name}-%{_revision}/rel/%{name}/erts-* \
		%{buildroot}%{riak_lib}
cp -r $RPM_BUILD_DIR/%{name}-%{_revision}/rel/%{name}/releases \
		%{buildroot}%{riak_lib}
cp -r $RPM_BUILD_DIR/%{name}-%{_revision}/doc/man/man1/*.gz \
		%{buildroot}%{_mandir}/man1
install -p -D -m 0644 \
	$RPM_BUILD_DIR/%{name}-%{_revision}/rel/%{name}/etc/app.config \
	%{buildroot}%{_sysconfdir}/%{name}/
install -p -D -m 0644 \
	$RPM_BUILD_DIR/%{name}-%{_revision}/rel/%{name}/etc/vm.args \
	%{buildroot}%{_sysconfdir}/%{name}/
install -p -D -m 0755 \
	$RPM_BUILD_DIR/%{name}-%{_revision}/rel/%{name}/bin/%{name} \
	%{buildroot}/%{_sbindir}/%{name}
install -p -D -m 0755 \
	$RPM_BUILD_DIR/%{name}-%{_revision}/rel/%{name}/bin/%{name}-admin \
	%{buildroot}/%{_sbindir}/%{name}-admin
install -p -D -m 0755 %{SOURCE1} %{buildroot}/%{init_script}

# Needed to work around check-rpaths which seems to be hardcoded into recent
# RPM releases
export QA_RPATHS=3


%pre
# create riak group only if it doesn't already exist
if ! getent group riak >/dev/null 2>&1; then
        groupadd -r riak
fi

# create riak user only if it doesn't already exist
if ! getent passwd riak >/dev/null 2>&1; then
        useradd -r -g riak --home %{_localstatedir}/lib/%{name} riak
        usermod -c "Riak Server" riak
fi

%files
%defattr(-,riak,riak)
%attr(-,root,root) %{_libdir}/*
%dir %{_sysconfdir}/%{name}
%config(noreplace) %{_sysconfdir}/%{name}/*
%attr(0755,root,root) %{init_script}
%attr(0755,root,root) %{_sbindir}/%{name}
%attr(0755,root,root) %{_sbindir}/%{name}-admin
%attr(0644,root,root) %{_mandir}/man1/*
%{_localstatedir}/lib/%{name}
%{_localstatedir}/log/%{name}
%{_localstatedir}/run/%{name}

%clean
rm -rf %{buildroot}

%changelog
* Tue Sep 21 2010 Grant Schofield <grant@basho.com> 0.13-1
- Added init.d script 

* Wed May 12 2010 Ryan Tilder <rtilder@basho.com> 0.10.1-3
- Tweak how bin/riak, bin/riak-admin, and bin/app.config are being modified
  for packaging

* Wed Mar 24 2010 Ryan Tilder <rtilder@basho.com> 0.9.1-2
- Some simplification and tweaks

* Wed Mar 10 2010 Grant Schofield <grant@basho.com> 0.9.0-1
- First 0.9 build

