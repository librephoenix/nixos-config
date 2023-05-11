#!/usr/bin/env python3

import xml.etree.ElementTree as ET
import copy

# Read content.xml into parser
mytree = ET.parse('./content.xml')
myroot = mytree.getroot()

# Read styles.xml into parser
styletree = ET.parse('./styles.xml')
styleroot = styletree.getroot()

# Remove direct-formatting from text:style-name attributes in text:p elements
counter = 0
for text in myroot.iter('{urn:oasis:names:tc:opendocument:xmlns:text:1.0}p'):
    if '{urn:oasis:names:tc:opendocument:xmlns:text:1.0}style-name' in text.keys():
        stylename = text.attrib['{urn:oasis:names:tc:opendocument:xmlns:text:1.0}style-name']
        if stylename[0] == "P":
            counter += 1
            text.attrib.pop('{urn:oasis:names:tc:opendocument:xmlns:text:1.0}style-name')
print('Deleted '+str(counter)+' text:style-name attributes in text:p elements.')

# Remove direct-formatting from text:style-name attributes in text:span elements
counter = 0
for span in myroot.iter('{urn:oasis:names:tc:opendocument:xmlns:text:1.0}span'):
    if '{urn:oasis:names:tc:opendocument:xmlns:text:1.0}style-name' in span.keys():
        span.attrib.pop('{urn:oasis:names:tc:opendocument:xmlns:text:1.0}style-name')
print('Deleted '+str(counter)+' text:style-name attributes in text:span elements.')

# Remove direct-formatting from draw:text-style-name attributes in draw:frame elements
counter = 0
for drawing in myroot.iter('{urn:oasis:names:tc:opendocument:xmlns:drawing:1.0}frame'):
    if '{urn:oasis:names:tc:opendocument:xmlns:drawing:1.0}text-style-name' in drawing.keys():
        stylename = drawing.attrib['{urn:oasis:names:tc:opendocument:xmlns:drawing:1.0}text-style-name']
        if stylename[0] == "P":
            counter += 1
            drawing.attrib.pop('{urn:oasis:names:tc:opendocument:xmlns:drawing:1.0}text-style-name')
print('Deleted '+str(counter)+' draw:text-style-name attributes in text:p elements.')

# Redefine default styles (style:style elements) and purge unnecessary ones
counter = 0
kounter = 0
for style in myroot.iter('{urn:oasis:names:tc:opendocument:xmlns:style:1.0}style'):
    if '{urn:oasis:names:tc:opendocument:xmlns:style:1.0}name' in style.keys():
        stylename = style.attrib['{urn:oasis:names:tc:opendocument:xmlns:style:1.0}name']
        if stylename == "pr1":
            counter += 1
            style.set('{urn:oasis:names:tc:opendocument:xmlns:style:1.0}parent-style-name','DefaultTheme-title')
        elif stylename == "pr2":
            counter += 1
            style.set('{urn:oasis:names:tc:opendocument:xmlns:style:1.0}parent-style-name','DefaultTheme-subtitle')
        elif stylename == "pr3":
            counter += 1
            style.set('{urn:oasis:names:tc:opendocument:xmlns:style:1.0}parent-style-name','DefaultTheme-notes')
        elif stylename == "pr4":
            counter += 1
            style.set('{urn:oasis:names:tc:opendocument:xmlns:style:1.0}parent-style-name','DefaultTheme-outline1')
print('Redefined '+str(counter)+' style:parent-style-name attributes in style:style elements.')
print('Deleted '+str(kounter)+' style:style elements.')

# Search for automatic-styles element
i = 0
col1 = 0
while (i < len(myroot)):
    print(myroot[i].tag)
    if myroot[i].tag=="{urn:oasis:names:tc:opendocument:xmlns:office:1.0}automatic-styles":
        col1 = i
    i += 1

# Remove unnecessary style:style and test:list-style elements underneath automatic-styles
i = 0
while (i < len(myroot[col1])):
    if (myroot[col1][i].tag == "{urn:oasis:names:tc:opendocument:xmlns:style:1.0}style"):
        if ("{urn:oasis:names:tc:opendocument:xmlns:style:1.0}name" in myroot[col1][i].keys()):
            if myroot[col1][i].attrib["{urn:oasis:names:tc:opendocument:xmlns:style:1.0}name"] in ["pr5","pr6","pr7","pr8","pr9"]:
                print("Removing "+myroot[col1][i].tag)
                myroot[col1].remove(myroot[col1][i])
                i -= 1
            elif myroot[col1][i].attrib["{urn:oasis:names:tc:opendocument:xmlns:style:1.0}name"][0] == "P":
                print("Removing "+myroot[col1][i].tag)
                myroot[col1].remove(myroot[col1][i])
                i -= 1
    if (myroot[col1][i].tag == "{urn:oasis:names:tc:opendocument:xmlns:text:1.0}list-style"):
        print("Removing "+myroot[col1][i].tag)
        myroot[col1].remove(myroot[col1][i])
        i -= 1
    i += 1

#i = 0
#while (i < len(myroot[col1])):
#    print(myroot[col1][i].attrib)
#    i += 1

# Find ML1 in styles.xml and copy it into L1 in content.xml
# Search for automatic-styles element
i = 0
stylecol1 = 0
while (i < len(styleroot)):
    print(styleroot[i].tag)
    if styleroot[i].tag=="{urn:oasis:names:tc:opendocument:xmlns:office:1.0}automatic-styles":
        stylecol1 = i
    i += 1

# Remove unnecessary style:style and test:list-style elements underneath automatic-styles
i = 0
while (i < len(styleroot[stylecol1])):
    if (styleroot[stylecol1][i].tag == "{urn:oasis:names:tc:opendocument:xmlns:text:1.0}list-style"):
        if (styleroot[stylecol1][i].attrib["{urn:oasis:names:tc:opendocument:xmlns:style:1.0}name"] == "ML1"):
            liststyle_copy = copy.deepcopy(styleroot[stylecol1][i])
            myroot[col1].append(liststyle_copy)
            myroot[col1][-1].attrib['{urn:oasis:names:tc:opendocument:xmlns:style:1.0}name'] = "L1"
    i += 1

# Update presentation:style-name attribute of all draw:frame elements
counter = 0
for frame in myroot.iter('{urn:oasis:names:tc:opendocument:xmlns:drawing:1.0}frame'):
    if '{urn:oasis:names:tc:opendocument:xmlns:presentation:1.0}class' in frame.keys():
        classname = frame.attrib['{urn:oasis:names:tc:opendocument:xmlns:presentation:1.0}class']
        if classname == "title":
            counter += 1
            frame.set('{urn:oasis:names:tc:opendocument:xmlns:presentation:1.0}style-name','pr1')
        elif classname == "subtitle":
            counter += 1
            frame.set('{urn:oasis:names:tc:opendocument:xmlns:presentation:1.0}style-name','pr2')
        elif classname == "notes":
            counter += 1
            frame.set('{urn:oasis:names:tc:opendocument:xmlns:presentation:1.0}style-name','pr3')
        elif classname == "outline":
            counter += 1
            frame.set('{urn:oasis:names:tc:opendocument:xmlns:presentation:1.0}style-name','pr4')
print("Updated "+str(counter)+" draw:frame elements")

# Update draw:master-page-name attributes in all draw:page elements
# Also delete all presentation:presentation-page-layout attributes
counter = 0
for page in myroot.iter('{urn:oasis:names:tc:opendocument:xmlns:drawing:1.0}page'):
    if '{urn:oasis:names:tc:opendocument:xmlns:drawing:1.0}master-page-name' in page.keys():
        page.set('{urn:oasis:names:tc:opendocument:xmlns:drawing:1.0}master-page-name','DefaultTheme')
        counter += 1
    if '{urn:oasis:names:tc:opendocument:xmlns:presentation:1.0}presentation-page-layout' in page.keys():
        page.attrib.pop('{urn:oasis:names:tc:opendocument:xmlns:presentation:1.0}presentation-page-layout')

print("Updated "+str(counter)+" draw:page elements")

# Update all text:list elements to have text:style-name = L1
counter = 0
for page in myroot.iter('{urn:oasis:names:tc:opendocument:xmlns:text:1.0}list'):
    if '{urn:oasis:names:tc:opendocument:xmlns:text:1.0}style-name' in page.keys():
        page.set('{urn:oasis:names:tc:opendocument:xmlns:text:1.0}style-name','L1')
        counter += 1

print("Updated "+str(counter)+" text:list elements")

#mytree.canonicalize(out='content.xml')
mytree.write('content.xml')
styletree.write('styles.xml')
