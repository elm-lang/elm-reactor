#!/usr/bin/env python

import os
import sys


## FIGURE OUT NEW MODIFICATION TIME

def mostRecentModification(directory):
	mostRecent = 0

	for dirpath, dirs, files in os.walk(directory):
		for f in files:
			lastModified = os.path.getmtime(dirpath + '/' + f)
			mostRecent = max(int(lastModified), mostRecent)

	return mostRecent


pageTime = mostRecentModification('src/pages')
assetTime = mostRecentModification('assets')
debuggerTime = mostRecentModification('src/debugger')
mostRecent = max(pageTime, assetTime, debuggerTime)


## FIGURE OUT OLD MODIFICATION TIME

with open('last-modified', 'a') as handle:
	pass


prevMostRecent = 0


with open('last-modified', 'r+') as handle:
	line = handle.read()
	prevMostRecent = int(line) if line else 0


## TOUCH FILES IF NECESSARY

if mostRecent > prevMostRecent:
	print ""
	print "--------------------------------------------------------------------------------"
	print "Some frontend file has been modified. Touching 'src/backend/StaticFiles.hs' to"
	print "trigger a recompilation of the Template Haskell stuff."
	print "--------------------------------------------------------------------------------"
	print ""
	os.utime('src/backend/StaticFiles.hs', None)
	with open('last-modified', 'w') as handle:
		handle.write(str(mostRecent))


## RUN CABAL BUILD

exitCode = os.system("cabal build")

sys.exit(1 if exitCode else 0)
