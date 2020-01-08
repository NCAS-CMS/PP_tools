# PP_tools

NCAS-CMS PP file tools

## pp_merge 

Merges (interleaves) PP files to produce a PP file in time order. This is suitable for use with NDdiag, which requires its input to be in time order.

Usage:

```
pp_merge {PP output file} { List of PP input files }
```

## pp_rdiff 

This compares two PP files of the same size. It computes the relative difference between corresponding field points according to the formula

![](rel_diff_equn.gif)


Usage:

```
pp_rdiff {Input PP file 1} {Input PP file 2} [{Threshold}]
```

where ```Threshold``` is a positive number. If no threshold is supplied, a value of 10^-7^ is used. The tool reports whether the files are different to within threshold and the maximum absolute relative difference found.

## pp_qdiff 

PP quick difference. This simply subtracts the field points of the two input files, which must be the same size, and puts them in the output.

Usage:

```
pp_qdiff {PP file 1} {PP file 2} {Output PP file}
```

The output file can then be examined by a graphical program like ```xconv```.

## pp_getfields 

This program extracts a list of fields specified by the user.

Usage:

```
pp_getfield {Input PP} {Output PP} {STASH list}
```

where the STASH items are separated by spaces.

## pp_unpack

BADC files are stored in WGDOS packed format (run ```ppinfo``` on them to see this). This tool unpacks them for use with other tools such as ```pp_merge``` and [wiki:ToolsAndUtilities/NDdiag NDdiag].

Usage:

```
pp_unpack {list of PP files}
```

Each input file is unpacked and stored in a file with ```.unpacked``` appended to the filename. For example, to process a collection of PP files for use with NDdiag:

```
pp_unpack *.pp
pp_merge out.pp *.unpacked
```
 
will create the file ```out.pp``` ready for NDdiag.
