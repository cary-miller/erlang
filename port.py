#!/usr/bin/env python

import sys
import os
import time
import requests


def serve():
    '''
    '''
    name = ''
    url = ''
    while 1:
        line = sys.stdin.readline().strip()
        if not line: break
        if line == 'name':  
            print name
        elif line.startswith('name'):
            name = ' '.join(line.split()[1:])
            print 'OK assigning name ', name
        elif line.startswith('http://'):
            url = line
#            url = ' '.join(line.split()[1:])
            print 'OK %s fetching url  %s' %(os.getpid(), url)
            response = requests.get(url)
            print 'status ', response.status_code
        elif line == 'status':
            print response.status_code
        elif line == 'text':
            print response.text

        else:
            response = "OK %s %s  Received %s" %(time.ctime(), os.getpid(), line.strip())
            print response
        sys.stdout.flush()

    print "alas... jumped out.  Goodbye."
    sys.stdout.flush()

serve()



