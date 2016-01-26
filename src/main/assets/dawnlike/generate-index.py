#!/usr/bin/env python

# This script generates an index for the DawnLike
# tiles based on several sources of data. It looks
# at a Nethack map file (nethack.map), searches
# for PNG files and combines some special logic
# to produce an index of all known tiles.

import collections, json, os, re, struct, sys

# All data is read relative to the location of this
# script file. The script file should be placed in
# the DawnLike assets directory.
curdir = os.path.dirname(os.path.abspath(__file__))

#############################
### Read Nethack map file ###
#############################

# Read the Nethack map file. The file is originally
# named 'dawnlike.map' and downloaded from the Nethack
# sources.
mapfile_path = os.path.join(curdir, 'nethack.map')

# Store the parsed Nethack map file entries in this
# list.
nethack_mapping = []

with open(mapfile_path, 'r') as f:

  # Rewrite a Nethack item mapping name so that any
  # 'sub xxx yyy' values become, 'xxx/yyy'. E.g.
  # 'sub ran sub questgoal solid rock' will become
  # 'ran/questgoal/solid rock'.
  def rewrite_sub_names(name):
    if name.startswith('sub '):
      name = name[4:]
      head, tail = name.split(' ', 1)
      return head + '/' + rewrite_sub_names(tail)
    else:
      return name

  # A pattern for extracting values from the Nethack
  # map file.
  regex = re.compile(r'(.*):(.*)\[(.*)\.png\] (0x.*)')

  for line in f.readlines():
    line = line.strip()
    # Ignore empty lines and comments.
    if len(line) == 0 or line[0] == '!': continue
    match = regex.match(line)

    # Parse the name, splitting it and rewriting 'sub'
    # expressions.
    namesString = match.group(1)
    if ',' in namesString:
      names = namesString.split(', ')
    elif ';' in namesString:
      names = namesString.split('; ')
    else:
      names = [namesString]
    names = map(rewrite_sub_names, names)

    # Parse the filename, removing trailing frame
    # numbers.
    filename = match.group(3)
    if filename[-1] == '0':
      filename = filename[:-1]

    # Parse the tile index, which is in hex format.
    index = int(match.group(4), 16)

    nethack_mapping.append((filename, index, names))

#######################################
### Read DawnLike tileset PNG files ###
#######################################

# Store all the tileset information in this set. We
# use a set because we'll add the same value more than
# once when a tileset has multiple PNG frame files.
tilesets = set()

# Read the width and height of a PNG file by parsing
# its header. The code for this came from Stack Overflow,
# see: http://stackoverflow.com/a/20380514
def png_dimensions(pngpath):
  with open(entrypath, 'rb') as pngfile:
    header = pngfile.read(24)
    assert struct.unpack('>i', header[4:8])[0] == 0x0d0a1a0a
    width, height = struct.unpack('>ii', header[16:24])
  _, filename = os.path.split(pngpath)
  # Ignore comment at bottom of Reptile tileset
  if filename.startswith('Reptile'):
    height -= 40
  assert width % 16 == 0 and height % 16 == 0
  tilewidth = width / 16
  tileheight = height / 16
  return (tilewidth, tileheight)

# The list of directories where the DawnLike tiles are
# stored. We list them explicitly because other directories
# might contain PNG files that aren't tilesets.
dirnames = ['Characters', 'Commissions', 'GUI', 'Items', 'Objects']
for dirname in dirnames:
  tiledir = os.path.join(curdir, dirname)
  for entry in os.listdir(tiledir):
    entrypath = os.path.join(tiledir, entry)
    if entry.endswith('.png') and os.path.isfile(entrypath):
      # Strip '.png' from the filename.
      entry = entry[:-4]

      # The presence of a number at the end of the filename
      # means that this file is a tileset with more than one
      # frame of animation. Strip the number and record that
      # the tileset is animated.
      if entry[-1] == '0' or entry[-1] == '1':
        entry = entry[:-1]
        animated = True
      else:
        animated = False
      dimensions = png_dimensions(entrypath)
      record = (dirname, entry, dimensions, animated)
      tilesets.add(record)

####################################
### Generate the JSON index file ###
####################################

# Utility to return a sorted copy of a collection.
# Unlike list.sort() a copy is returned.
def sorted(collection):
  l = list(collection)
  l.sort()
  return l

# The root JSON contains an entry for each tileset
# directory.
root_json = collections.OrderedDict()
for dirname in dirnames:
  dir_json = collections.OrderedDict()
  root_json[dirname] = dir_json

  # Each directory is a JSON object with an entry for
  # each tileset.
  tilesets_in_dir = [t for t in tilesets if t[0] == dirname]
  for _, tileset, dimensions, animated in sorted(tilesets_in_dir):
    tileset_json = collections.OrderedDict()
    dir_json[tileset] = tileset_json

    tileset_width = dimensions[0]
    tileset_height = dimensions[1]
    tileset_json['width'] = tileset_width
    tileset_json['height'] = tileset_height
    tileset_json['animated'] = animated

    # Each tileset has a list of its tiles.
    tilelist_json = []
    tileset_json['tiles'] = tilelist_json

    # Filter the Nethack tiles to get our entries. Sort the tiles by
    # their index in the tilemap so that they end up grouped in a
    # natural order.
    nethack_tiles = sorted([t for t in nethack_mapping if t[0] == tileset])
    for _, index, names in nethack_tiles:
      tile_json = collections.OrderedDict()
      tilelist_json.append(tile_json)
      tile_x = index % tileset_width
      tile_y = index / tileset_width
      assert tile_y < tileset_height
      # List y before x because that's the natural way to sort them.
      tile_json['y'] = tile_y
      tile_json['x'] = tile_x

      # Store the Nethack mapping names as an attribute in each tile's
      # JSON. This means we can look up the tiles by their Nethack name
      # if we want.
      attrs_json = collections.OrderedDict()
      tile_json['attrs'] = attrs_json

      # Attributes can be either a single string or an array of strings.
      # Try to use a single string if possible.
      if len(names) == 1:
        nethack_attr_json = names[0]
      else:
        nethack_attr_json = names
      attrs_json['nethack'] = nethack_attr_json

print json.dumps(root_json, indent=2)
