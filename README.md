##ProjectInstaller

`ProjectInstaller` is a simple web installer for Mathematica projects. 
The projects can be stored on the web or on the local machine, in .zip 
or .tar.gz formats. It attempts to download and install a given project 
into a specified project directory.

### Project's format

There are only a few requirements for the directory structure of the 
project being installed. If it is stored in a .zip archive, there 
must be exactly one top-level project directory in the archive, and
nothing else. If it is stored in a .tar.gz format,the .gz archive 
should contain exactly one .tar file and nothing else, and the .tar
file should contain exactly one project's folder, plus it may contain
some files, which will be ignored by the installer.

The project's folder itself should contain at least one package (.m)
file, plus possibly other files and /or folders. If there is only one
package file, it should be the same as the name of the project. 

The project may optionally contain a project descriptor file 
`project.m`, which determines the properties of the project.
Here is the contents of a sample `project.m` file:

    {
        "author" ->
           {
               "name" -> "Leonid Shifrin",
               "email" -> "lshifr@gmail.com",
               "url" -> "http://www.mathprogramming-intro.org"
           },           
        "name" -> "Test1",
        "version" -> "1.0",
        "mathematica_version" -> "8.0+",
        "dependencies" -> {},
        "description"-> "Simple test project for the installer",
        "url" -> "https://gist.github.com/gists/4143517/download",
        "license" ->  "MIT"
    }
    
The data here must be a list of rules, and in the rules one can use
strings and numbers. Basically, the content of the `project.m` should 
be exportable to JSON via `ExportString[#,"JSON"]&`.

Not all fields are mandatory. At present, only the `name` field is 
effectively used by the installer, but in the future more fields will
be utilized.

If there is more than one package in the top-level project's folder, 
then the project *must* contain a valid project descriptor file, with
the `name` field set to the name of one of the top-level packages.

Note that the installer (at least in the current version) will always
create a `Kernel` folder and place there the `init.m` file for the 
project. If the `init.m` file exists in the top-level directory of the
archived project, it will be simply copied. If it does not exist, it
will be auto-generated from the project's name and the name of the 
main package in the project (which, at least currently, are required
to be the same). 



###Installation

The installation procedure is standard, as for any *Mathematica*
package (alas, the installer can not install itself, not for the 
first time at least):

 - Download the .zip archive with the project and extract it so 
some folder.
 - Copy it into one of the directories where *Mathematica* 
can find it, for example in a directory returned by evaluating
`FileNameJoin[$UserBaseDirectory,"Applications"]`
 - Call ``Needs["ProjectInstaller`"]``

###How to use

To install the project from the web, use 

    ProjectInstall[URL[string-url-of-the-project]]
    
To install the project from a local file, use

    ProjectInstall[path]
    
where `path` must be a string pointing to an existing .zip or
.tar.gz archive on your local file system.

Note that the project won't be installed if a project with the 
same name does already exist in your project repository.

To remove (uninstall) the project, use

    ProjectUninstall[name_String]
    
where `name` is the name of the project (not the full path to it).
This will result into the project being removed from your project
repository. 

The default project repository location is at 

    $UserBaseDirectory/Applications
    
You can specify different location for it, using the `DestinationDirectory`
option, which is accepted by both `ProjectInstall` and `ProjectUninstall`.
Note however that it is up to you to make Mathematica recognize the non -
default repository (by adding it to `$Path`, e.g.).

    
###Examples

The following

    ProjectInstall[URL["https://github.com/lshifr/CodeFormatter/archive/master.zip"]]

should install the code formatter project to your repository. You can start 
using it, e.g. by calling

    Needs["CodeFormatter`"]
    
and then try on some function's code, e.g. 

    CellPrint[Cell[#, "Input"]] &@FullCodeFormat[
       MakeBoxes[
         ClearAll[ReverseIfFails];
         SetAttributes[ReverseIfFails, HoldFirst];
         ReverseIfFails[initCode_, mainFunc_, reverseFunc_] := 
           Module[{init, result}, 
            If[(init = initCode) === $Failed, Return[$Failed]];
             CleanUp[result = mainFunc[init], 
              If[result === $Failed, reverseFunc[init]]]];
       ]]
       
The following will uninstall the formatter:

    ProjectUninstall["CodeFormatter"]



###License

This package is released under MIT Open Source license. The copy of the license can be found [in the project](https://github.com/lshifr/ProjectInstaller/blob/master/LICENSE) 


