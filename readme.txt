Chee Wee's Productivity Experts 0.8
-----------------------------------


Introduction
-------------

This is an IDE keyboard binding expert that provides the following
functions.
1) Region Expert - marks a block of selected text as a region with a
   keystroke in C# and Delphi.
2) Comment Expert - marks a block of selected text as comments with a
   keystroke in C# and Delphi.
3) generates XML-styled comments (XML Doc) for code declarations with a key
   sequence in Delphi and C#.
4) auto comments extension expert - automatically inserts // in between
   blocks of lines in Delphi.
5) Goto Region Expert - iterates through all open files and presents a list
   of regions, allowing you to select what region to go to, optionally expanding
   the region, and displaying the selected region at the top of the editor.
   Currently only supports Delphi as there's a bug in the OTAPI which doesn't
   allow it to iterate C# regions. Activate using Ctrl O + Ctrl X ( NOT Ctrl O + X ).
6) Code Template Insight - supports all personalities/languages
   Automatically expands any matched code templates. Code Template Names
   may not differ by 1 letter, eg, tryc, trycf. If that happens, the first
   match is always taken, ie, the name of a code template should not be the prefix
   of another. Instead, use something like tryc1, trycf. To try in Delphi, try 
   typing arrayc. To try in C#, try typing tryc.
7) MSDN Help expert - provides help using Ctrl F1 by looking up in MSDN with the word
   nearest the cursor.

Installation/Compilation instructions
-------------------------------------
There are two ways to install.

Method One
----------
Use Install Component, and select the package to install.

Method Two
----------
1) Load the package (.dpk) or project (.bdsproj) in the IDE (if available).
2) Activate Project Manager.
3) Choose Install, which will compile and install the package into the IDE.


Assumptions
-----------

The experts were tested in Insert mode only. Results are not guaranteed in
Overwrite mode.

Usage instructions
------------------

1) In order to use the region expert, select a block of text, and press
   Ctrl D. You will then be prompted for the region block name. If you
   cancel, nothing happens. Once you've entered a region name, and
   pressed/clicked Ok, the block of text is region-ised.

   The selected block of text will then be marked as such

   {$REGION 'User provided region name'}
    ... selected text...
   {$ENDREGION}

2) In order to use the comment expert, select a block of text, and press
   Ctrl K /

   The selected block of text will then be marked as such

   // ... selected text...

3) In order to use the XML styled code comments expert, press / (forward
   slash) 3 times before a routine declaration. You can either do it before
   a routine declaration on the same line, or on the line before the
   routine declaration.

   Examples
   1) |procedure X(Y: Integer);
   2) |
      procedure X(Y: Integer);

4) In order to use the auto comments extension expert, press enter at the
   end of a comment line.

   For example, given the following,
   line 1:   // Comment 1
   line 2:   // Comment 2

   and you press enter at the line 1, then the following happens
   line 1:   // Comment 1
   line 2:   //
   line 3:   // Comment 2


5) In order to use the Goto Region expert, open all Delphi (.pas) files which contains 
   regions. Then, press Ctrl O + Ctrl X ( NOT Ctrl O + X) and a list of regions will
   be presented. Either double click on the region, or click on the region and press Enter.
   You will then go to the selected region, optionally expanded at the top of the editor.

Enjoy,
Chua Chee Wee,
Singapore
12 May 2005.