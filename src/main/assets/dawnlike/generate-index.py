#!/usr/bin/env python

# This script generates an index for the DawnLike
# tiles based on several sources of data. It looks
# at a Nethack map file (nethack.map), searches
# for PNG files and combines some special logic
# to produce an index of all known tiles.

import collections, json, os, pprint, re, struct, sys

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
nethack_tile_attrs = collections.OrderedDict()

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

    key = (filename, index)
    nethack_tile_attrs[key] = names

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

###############################################
### Generate entries for floors, walls, etc ###
###############################################

generated_tile_attrs = collections.OrderedDict()

floor_pattern = [
  ['tl', 't', 'tr', 'trl', None, 'trbl', None],
  ['l', 'fill', 'r', 'rl', 'tbl', 'tb', 'trb'],
  ['bl', 'b', 'rb', 'rbl', None, None, None]
]

stone_colors = ('sky', 'slate', 'olive', 'iron')
dirt_colors = ('peach', 'ocher', 'earth', 'iron')
wood_colors = ('peach', 'ocher', 'olive', 'earth')

floor_groups = [
  [
    ('stone', stone_colors, 'edge', stone_colors),
    ('grass', ('sky', 'leaf', 'olive', 'midnight'), 'dirt', dirt_colors),
    ('rock', dirt_colors, 'dirt', dirt_colors)
  ],
  [
    ('dirt', dirt_colors, 'edge', ('maize', 'peach', 'berry', 'earth')),
    ('wood', wood_colors, 'edge', wood_colors),
    ('sand', ('peppermint/maize', 'peppermint/sky', 'sky', 'slate'), 'dirt', dirt_colors)
  ],
  [
    ('furrows', ('ocher', 'earth', 'iron', 'midnight'), 'dirt', dirt_colors)
  ]
]

skip_rows = 3 # Skip documentation at top of floor tileset
num_color_rows_per_group = len(stone_colors)
num_pattern_rows_per_color = len(floor_pattern)
num_pattern_cols_per_group = len(floor_pattern[0])

for group_row, group_row_list in enumerate(floor_groups):
  for group_col, (main, main_colors, other, other_colors) in enumerate(group_row_list):
    for color_row, (main_color, other_color) in enumerate(zip(main_colors, other_colors)):
      # common stuff....
      for pattern_row, pattern_row_list in enumerate(floor_pattern):
        for pattern_col, pattern_code in enumerate(pattern_row_list):
          if pattern_code is None:
            continue

          y = skip_rows + \
            (group_row * num_color_rows_per_group * num_pattern_rows_per_color) + \
            (color_row * num_pattern_rows_per_color) + \
            pattern_row
          x = (group_col * num_pattern_cols_per_group) + \
            pattern_col

          attrs = collections.OrderedDict()
          attrs['ground'] = main
          attrs['color'] = main_color

          # If the pattern is not 'fill' then it has edges.
          if pattern_code != 'fill':
            if other != 'edge':
              attrs['edge'] = other
            attrs['edge_dirs'] = pattern_code
            if other_color != main_color:
              attrs['edge_color'] = other_color

          key = ('Objects', 'Floor', y, x)
          generated_tile_attrs[key] = attrs

wall_pattern = [
  ['rb', 'rl', 'bl', 'fill', 'rbl', None],
  ['tb', 'pillar', None, 'trb', 'trbl', 'trb'],
  ['tr', None, 'tl', None, 'trl', None]
]

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

    for tile_y in xrange(tileset_height):
      for tile_x in xrange(tileset_width):
        tile_json = collections.OrderedDict()

        # List y before x because that's the natural way to sort them.
        tile_json['y'] = tile_y
        tile_json['x'] = tile_x

        attrs_json = collections.OrderedDict()
        tile_json['attrs'] = attrs_json

        # Look to see if we have any Nethack names for this tile.
        # If we do, set the names as an attribute on the tile.
        nethack_key = (tileset, tile_y * tileset_width + tile_x)
        nethack_names = nethack_tile_attrs.get(nethack_key, None)
        if nethack_names is not None:
          # Attributes can be either a single string or an array of strings.
          # Try to use a single string if possible.
          if len(nethack_names) == 1:
            nethack_attr_json = nethack_names[0]
          else:
            nethack_attr_json = nethack_names
          attrs_json['nethack'] = nethack_attr_json

        # Look to see if we have any generated attributes for this
        # tile. If we do, set them on the attributes.
        generated_key = (dirname, tileset, tile_y, tile_x)
        generated_attrs = generated_tile_attrs.get(generated_key, {})
        # if len(generated_attrs) > 0: print generated_attrs
        attrs_json.update(generated_attrs)

        # Don't bother outputting the tile info unless we have some
        # attributes.
        if len(attrs_json) > 0:
          tilelist_json.append(tile_json)

print json.dumps(root_json, indent=2)
