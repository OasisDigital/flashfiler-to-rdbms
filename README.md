# FlashFiler to RDBMS Converter

Copyright 2013 Oasis Digital Solutions Inc. All right reserved.

Released under the Apache 2 license, see LICENSE file.

## Explanation

In our software development work at Oasis Digital, we have occasionally
needed to translate data between old file-based databases and relational
database implementations. In such work we developed this tool to
make it easier to repeatedly translate such data from FlashFiler to
MS SQL Server or PostgreSQL.

It is very unusual to encounter a project still using an older database
file type like this, but we release this tool in hopes that it may help
someone else upgrade their systems.

For more explanation, see the following page.

http://delphi.oasisdigital.com/flashfiler-to-sql-server-postgresql-data-converter/

## Usage

Please see FlashFilertoRDBMSConverterDocumentation.pdf

## Build notes

1. Download and installed latest FlashFiler from SourceForge into Delphi 7 Enterprise,
   using their supplied readme.txt as a guide. There is a bug in the ff2_d70.dpk;
   change its requires reference of ff213_r70 to ff2_r70 and then it will build and install.

2. If you don't have installed TShellTreeView, you will need to install it at first.
   Update your Delphi environment settings to add the path
   "%ProgramFiles%\Borland\Delphi7\Demos\ShellControls".
   Build the vclshlctrls.dpk runtime package,
   and then build and install the dclshlctrls.dpk designtime package.

3. The project uses the FlashFiler source code.
    Update your Delphi project options to add the search path:
    "..\TurboPower FlashFiler\source"
