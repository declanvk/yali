# script that produced the backtrace
# sudo lldb -f /Users/dev/Projects/walox/target/debug/deps/filecheck_interpreter-9888d9585a8e0977 -b -o 'run' -k 'thread backtrace -e true' -k 'kill' -k 'quit' -- operator::equals_method -k 'quit' > lldb-stdout.txt

import re
from collections import namedtuple

frame_details_reg = re.compile(r"""
    # capture the frame number
    ^\s*frame\s+\#(?P<f_number>\d+):\s+
    # capture the frame address in hex
    (?P<f_addr>0x[\d\w]+)\s+
    filecheck_interpreter-\w+`
    (?P<rest>.*)$
""", re.VERBOSE)

frame_rest_reg = re.compile(r"""
    ^(?P<path>.*?)
    (?P<args>\(.*\))?
    (\s+at\s+(?P<file>.*))
""", re.VERBOSE)

full_trace = list()

FDetails = namedtuple('FDetails', ['number', 'address', 'rest'])

FRest = namedtuple('FRest', ['path', 'args', 'file'])

with open('lldb-stdout.txt', 'r') as fp:
    for cnt, line in enumerate(fp):
        m = frame_details_reg.match(line)
        if m is None:
            raise Exception(f"Unable to match frame regex to line {cnt}.")

        mrest = frame_rest_reg.match(m.group('rest'))
        if mrest is None:
            rest = FRest(m.group('rest'), None, None)
        else:
            rest = FRest(mrest.group('path'), mrest.group(
                'args'), mrest.group('file'))

        details = FDetails(int(m.group('f_number')),
                           int(m.group('f_addr'), 16),
                           rest)

        full_trace.append(details)
