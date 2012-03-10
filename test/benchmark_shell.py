#!/usr/bin/python
#
# This program is intended to be run via sudo, and does the following
# things:
#
# 1. Sets up a high-performance networking environment by tweaking
#    various sysctl settings.
# 2. Runs either an interactive shell or a command line program.
# 3. Resets the environment back to what it was.

import os, sys, tempfile

sysctls = dict(
    Darwin={
        'kern.ipc.somaxconn': 1024,
        'kern.maxfiles': 22528,
        'kern.maxfilesperproc': 20480,
        'net.inet.ip.portrange.first': 1024,
        'net.inet.ip.portrange.hifirst': 1024,
        },
    Linux={
        'net.core.somaxconn': 1024,
        'net.core.rmem_max': 16777216,
        'net.core.wmem_max': 16777216,
        'net.ipv4.ip_local_port_range': '1024 65535',
        'net.ipv4.tcp_fin_timeout': 15,
        'net.ipv4.tcp_max_syn_backlog': 16384,
        'net.ipv4.tcp_rmem': '4096 87380 16777216',
        'net.ipv4.tcp_tw_recycle': 1,
        'net.ipv4.tcp_tw_reuse': 1,
        'net.ipv4.tcp_wmem': '4096 65536 16777216',
        },
    )

ulimits = dict(
    Darwin={
        '-n': 20480,
        },
    Linux={
        '-n': 131072,
        },
    )

if os.access('/sbin/sysctl', os.X_OK):
    sysctl = '/sbin/sysctl'
elif os.access('/usr/sbin/sysctl', os.X_OK):
    sysctl = '/usr/sbin/sysctl'
else:
    print >> sys.stderr, 'where is sysctl!?'
    sys.exit(1)

changed_sysctls = {}

def change_sysctl(name, newval):
    oldval = os.popen('%s -n %s' % (sysctl, name), 'r').read().strip().replace('\t', ' ')
    if not oldval:
        print >> sys.stderr, 'could not get value of ' + name
        return
    if oldval == str(newval):
        return
    ret = os.system('%s -w %s=%r 2>/dev/null' % (sysctl, name, newval))
    if ret != 0:
        print >> sys.stderr, 'could not change %s from %s to %r' % (name, oldval, newval)
        return
    changed_sysctls[name] = oldval

platform = os.uname()[0]

for (n,v) in sysctls[platform].iteritems():
    change_sysctl(n,v)

fd, path = tempfile.mkstemp('.sh')

fp = os.fdopen(fd, 'w')
for (n, v) in ulimits[platform].iteritems():
    print >> fp, 'ulimit %s %s' % (n, v)
    if len(sys.argv) > 1:
        print >> fp, 'exec ' + ' '.join(sys.argv[1:])
    else:
        print >> fp, 'exec %s -l' % os.environ.get('SHELL', '/bin/bash')
fp.close()
os.system('exec /bin/sh ' + path)
os.unlink(path)

for (n,v) in changed_sysctls.iteritems():
    change_sysctl(n,v)
