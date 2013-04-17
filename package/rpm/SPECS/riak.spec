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
Source: %{name}-%{_revision}.tar.gz
Source1: riak_init
URL: http://basho.com
Vendor: Basho Technologies
Packager: Basho Technologies <riak-user@lists.basho.com>
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
Summary: Riak Distributed Data Store

requires: sudo

%description
Riak is a highly scalable, fault-tolerant distributed database

%define riak_lib %{_libdir}/%{name}
%define init_script %{_sysconfdir}/init.d/%{name}

%define __prelink_undo_cmd /bin/cat prelink library
%define debug_package %{nil}

%define platform_bin_dir %{_sbindir}
%define platform_data_dir %{_localstatedir}/lib/%{name}
%define platform_etc_dir %{_sysconfdir}/%{name}
%define platform_base_dir %{riak_lib}
%define platform_lib_dir %{platform_base_dir}/lib
%define platform_log_dir %{_localstatedir}/log/%{name}

%prep
%setup -q -n %{name}-%{_revision}

# Override the default vars.config with platform specific settings
cat > rel/vars.config <<EOF
% Platform-specific installation paths
{platform_bin_dir,  "%{platform_bin_dir}"}.
{platform_data_dir, "%{platform_data_dir}"}.
{platform_etc_dir,  "%{platform_etc_dir}"}.
{platform_base_dir, "%{platform_base_dir}"}.
{platform_lib_dir,  "%{platform_lib_dir}"}.
{platform_log_dir,  "%{platform_log_dir}"}.

%%
%% etc/app.config
%%
{web_ip,            "127.0.0.1"}.
{web_port,          8098}.
{handoff_port,      8099}.
{pb_ip,             "127.0.0.1"}.
{pb_port,           8087}.
{ring_state_dir,    "%{platform_data_dir}/ring"}.
{bitcask_data_root, "%{platform_data_dir}/bitcask"}.
{leveldb_data_root, "%{platform_data_dir}/leveldb"}.
{sasl_error_log,    "%{platform_log_dir}/sasl-error.log"}.
{sasl_log_dir,      "%{platform_log_dir}/sasl"}.

%% riak_search
{merge_index_data_root,  "%{platform_data_dir}/merge_index"}.

%% lager
{lager_handlers, "[ \
                           {lager_file_backend, [ \
                               {\"{{platform_log_dir}}/error.log\", error, 10485760, \"$D0\", 5}, \
                               {\"{{platform_log_dir}}/console.log\", info, 10485760, \"$D0\", 5} \
                           ]} \
                       ]"}.

%% Javascript VMs
{map_js_vms,   8}.
{reduce_js_vms, 6}.
{hook_js_vms, 2}.

%%
%% etc/vm.args
%%
{node,         "riak@127.0.0.1"}.
{crash_dump,   "%{platform_log_dir}/erl_crash.dump"}.

%%
%% bin/riak
%%
{runner_script_dir,  "%{platform_bin_dir}"}.
{runner_base_dir,    "%{platform_base_dir}"}.
{runner_etc_dir,     "%{platform_etc_dir}"}.
{runner_log_dir,     "%{platform_log_dir}"}.
{pipe_dir,           "/tmp/%{name}/"}.
{runner_user,        "%{name}"}.
EOF

# Set the 'riak version' output
cp rel/files/riak rel/files/riak.tmp
sed -e "s/^RIAK_VERSION.*$/RIAK_VERSION=\"%{_versionstring}\"/" < rel/files/riak.tmp > rel/files/riak

%build
mkdir %{name}
make rel

%install
mkdir -p %{buildroot}%{platform_etc_dir}
mkdir -p %{buildroot}%{platform_base_dir}
mkdir -p %{buildroot}%{platform_lib_dir}
mkdir -p %{buildroot}%{_mandir}/man1
mkdir -p %{buildroot}%{platform_data_dir}/dets
mkdir -p %{buildroot}%{platform_data_dir}/bitcask
mkdir -p %{buildroot}%{platform_data_dir}/leveldb
mkdir -p %{buildroot}%{platform_data_dir}/ring
mkdir -p %{buildroot}%{platform_data_dir}/merge_index
mkdir -p %{buildroot}%{platform_log_dir}
mkdir -p %{buildroot}%{platform_log_dir}/sasl
mkdir -p %{buildroot}%{_localstatedir}/run/%{name}
mkdir -p %{buildroot}%{platform_data_dir}/mr_queue

#Copy all necessary lib files etc.
cp -r $RPM_BUILD_DIR/%{name}-%{_revision}/rel/%{name}/lib %{buildroot}%{platform_base_dir}
cp -r $RPM_BUILD_DIR/%{name}-%{_revision}/rel/%{name}/erts-* \
                %{buildroot}%{platform_base_dir}
cp -r $RPM_BUILD_DIR/%{name}-%{_revision}/rel/%{name}/releases \
                %{buildroot}%{platform_base_dir}
cp -r $RPM_BUILD_DIR/%{name}-%{_revision}/doc/man/man1/*.gz \
                %{buildroot}%{_mandir}/man1
install -p -D -m 0644 \
        $RPM_BUILD_DIR/%{name}-%{_revision}/rel/%{name}/etc/app.config \
        %{buildroot}%{platform_etc_dir}/
install -p -D -m 0644 \
        $RPM_BUILD_DIR/%{name}-%{_revision}/rel/%{name}/etc/vm.args \
        %{buildroot}%{platform_etc_dir}/
install -p -D -m 0755 \
        $RPM_BUILD_DIR/%{name}-%{_revision}/rel/%{name}/bin/%{name} \
        %{buildroot}/%{platform_bin_dir}/%{name}
install -p -D -m 0755 \
        $RPM_BUILD_DIR/%{name}-%{_revision}/rel/%{name}/bin/%{name}-admin \
        %{buildroot}/%{platform_bin_dir}/%{name}-admin
install -p -D -m 0755 \
        $RPM_BUILD_DIR/%{name}-%{_revision}/rel/%{name}/bin/search-cmd \
        %{buildroot}/%{platform_bin_dir}/search-cmd
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
        useradd -r -g riak --home %{platform_data_dir} riak
        usermod -c "Riak Server" riak
fi

%post
# Fixup perms for SELinux
selinuxenabled && find %{platform_base_dir} -name "*.so" -exec chcon -t textrel_shlib_t {} \;

%files
%defattr(-,root,root)
%{_libdir}/*
%dir %{platform_etc_dir}
%config(noreplace) %{platform_etc_dir}/*
%{init_script}
%{platform_bin_dir}/%{name}
%{platform_bin_dir}/%{name}-admin
%{platform_bin_dir}/search-cmd
%{_mandir}/man1/*
%attr(-,riak,riak) %{platform_data_dir}
%attr(-,riak,riak) %{platform_log_dir}

%clean
rm -rf %{buildroot}
