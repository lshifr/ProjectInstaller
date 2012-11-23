(* Mathematica Package *)

(* Created by the Wolfram Workbench 21.11.2012 *)

BeginPackage["ProjectInstaller`", {"JLink`"}]
(* Exported symbols added here with SymbolName::usage *) 

Begin["`Private`"]
(* Implementation of the package *)

Needs["JLink`"];
Needs["ResourceLocator`"];


(* Todo: replace this with a Throw[$Failed] definition *)

$projectJarLocations = {
   "C:\\Users\\Archie\\WolframWorkspaces\\Alt\\ProjectInstaller\\\
javaOutput\\Build\\jar\\ProjectInstaller.jar",
   "C:\\Users\\Archie\\WolframWorkspaces\\Alt\\ProjectInstaller\\\
ProjectInstaller\\Java"
   };
   
$destinationDirectory = 
  FileNameJoin[{$UserBaseDirectory, "Applications"}];
  
  
SetAttributes[CleanUp, HoldAll];
CleanUp[expr_, cleanup_] :=
  Module[{exprFn, result, abort = False, rethrow = True, seq}, 
   exprFn[] := expr;
   result = 
    CheckAbort[
     Catch[Catch[result = exprFn[]; rethrow = False; result], _, 
      seq[##] &], abort = True];
   cleanup;
   If[abort, Abort[]];
   If[rethrow, Throw[result /. seq -> Sequence]];
   result];
   
   
(* TODO: decide whether throwing an exception is indeed appropriate \
here *)
ClearAll[shortCircuitFail];
SetAttributes[shortCircuitFail, HoldRest];
shortCircuitFail[funName_Symbol, code_, rest___] :=
  With[{res = code},
   If[res === $Failed,
    Throw[
     $Failed,
     error[
      funName,
      Hold[code] /. var_Symbol :> With[{result = var}, result /; True]
      ]], 
    (* else *)
    If[Length[Hold[rest]] == 0, res, shortCircuitFail[funName, rest]]
    ]];
    
    
    
ClearAll[check];
check[cond_] := If[! TrueQ[cond], $Failed];



ClearAll[makeTemporaryDirectoryName];
makeTemporaryDirectoryName::maxit = 
  "Unable to produce a unique directory name in `1` iterations";
Options[makeTemporaryDirectoryName] = {
   MaxIterations -> 1000
   };
   
makeTemporaryDirectoryName[globalTempDir_] :=
 Module[{tempDir, n = 0, lim = OptionValue[MaxIterations] },
  While[
   FileExistsQ[
     tempDir = 
      FileNameJoin[{
        globalTempDir,
        StringJoin["temp", ToString[RandomInteger[10^5]]]
        }]
     ] && ++n < lim];
  If[n > lim,
   Message[makeTemporaryDirectoryName::maxit, lim];
   Throw[$Failed, error[makeTemporaryDirectoryName]]
   ];
  tempDir]
       


(* TODO Add error messages / analysis, based on Java error info *)
ClearAll[extractArchive];
extractArchive::nojars = "Can not find the project's jars";
extractArchive[source_, dest_, type_: "ZIP"] :=
  Block[{unZip, unGzip, unTar},
   Module[{javaError, javaExtractor, jarlocations},
    (* Note: a global variable *)
    jarlocations = $projectJarLocations;
    If[! MatchQ[jarlocations, {__String}]
      || ! And @@ Map[
         DirectoryQ[#] || (FileExistsQ[#] && 
             ToLowerCase[FileExtension[#]] === "jar") &,
         jarlocations
         ]
      || AddToClassPath @@ jarlocations === $Failed,
     Message[extractArchive::nojars ];
     Throw[$Failed, error[extractArchive, "no_jars"]]
     ];
    JavaBlock[
     javaExtractor = Quiet@JavaNew["org.lshifr.arch.ZipTarExtractor"];
     If[javaExtractor === $Failed,
      Throw[$Failed, 
        error[extractArchive, "can_not_create_java_class"]];
      ];
     With[{method = 
        Switch[type,
         "ZIP",
         	unZip,
         "GZIP",
         	unGzip,
         "TAR",
         	unTar,
         _,
         	Throw[$Failed, error[extractArchive]]
         ]
       },
      javaError = javaExtractor@method[source, dest]
      ];
     If[javaError@msg =!= "Success",
      Throw[$Failed, error[extractArchive, javaError@msg]],
       dest
      ]
     ]]];


Clear[getProjectName];
getProjectName[prfile_] :=
 Get[prfile] /. 
  project[rules__] :> ("name" /. {rules} /. 
     "name" :> Throw[$Failed, getProjectName])
     
    
    
Clear[singlePackageInProjectQ];
singlePackageInProjectQ[prdir_] :=
  Length[FileNames["*.m", {prdir}]] == 1;
  
  
  
ClearAll[determineProjectName];
determineProjectName::nopckg = 
  "A package with the same name as the project: `1`, expected in the \
top level of the porject directory";
determineProjectName[prdir_String?DirectoryQ] :=
  Module[{prfile = FileNameJoin[{prdir, "project.m"}],
    packages = FileNames["*.m", {prdir}],
    prname},
   prname = Which[
     FileExistsQ[prfile],
     	getProjectName[prfile],
     Length[packages] == 1,
     	FileBaseName[First@packages],
     True,
     	Throw[$Failed, determineProjectName]
     ];
   With[{mainPackage = FileNameJoin[{prdir, prname <> ".m"}]},
    If[! MemberQ[packages, mainPackage],
     Message[determineProjectName::nopckg, 
      FileNameTake[mainPackage, -1]];
     Throw[$Failed, determineProjectName];
     ]];
   prname
   ];
   
   
   
ClearAll[projectAlreadyExistsQ];
projectAlreadyExistsQ[prdir_String?DirectoryQ, prname_String] :=
  Select[
    FileNames[{"*"}, prdir],
    FileBaseName[#] === 
       prname && (DirectoryQ[#] || FileExtension[#] === "m") &
    ] =!= {}; 
    
    
    
ClearAll[projectUninstall];
Options[projectUninstall] = {
   DestinationDirectory :> $destinationDirectory
   };
projectUninstall[prname_, opts : OptionsPattern[]] :=
  With[{prdir = OptionValue[DestinationDirectory]},
   DeleteDirectory[FileNameJoin[{prdir, prname}], 
     DeleteContents -> True] /; projectAlreadyExistsQ[prdir, prname]
   ];    
   
   

Clear[formatCode];
formatCode[code_Hold] := 
  StringReplace[
   Function[Null, ToString[Unevaluated[#], InputForm], HoldAll] @@ 
    code, ";" :> ";\n"];
    
    
    
Clear[saveCode];
saveCode[file_, generatedCode_] :=
  With[{result = BinaryWrite[file, formatCode@generatedCode]},
   Close[file];
   result];
   
   
   
ClearAll[makeContext];
makeContext[name_String, contextSeparator_String: "`"] := 
  name <> contextSeparator; 
  
  
  
ClearAll[makeCodeForInitFile];
makeCodeForInitFile[projectName_String] :=
 With[{context = # <> # &[makeContext[projectName] ]},
  Hold[Get[context]]
  ]
  
  
  
ClearAll[moveFile];
moveFile[src_String?FileExistsQ, dest_String] :=
  shortCircuitFail[
   moveFile,
   CopyFile[src, dest],
   DeleteFile[src],
   dest
   ];
moveFile[___] := Throw[$Failed, error[moveFile]];



ClearAll[exitOnFailure];
exitOnFailure[val_, fun_] :=
  If[val === $Failed, Throw[$Failed, error[fun]]];
           
         
         
ClearAll[initFileNameFor];
initFileNameFor[dir_String] :=
  FileNameJoin[{dir, "init.m"}];
  
  
  
Clear[makeInitFile];
(* It is assumed that by the time this function is called, all the \
files have been copied to the targetProjectDir *)
makeInitFile[targetProjectDir_String?DirectoryQ, projectName_String] :=  
  Module[{kernelDir, newInitFile, initFile},
   initFile = initFileNameFor[targetProjectDir];
   exitOnFailure[
    kernelDir = 
     CreateDirectory[FileNameJoin[{targetProjectDir, "Kernel"}]],
    makeInitFile
    ];
   newInitFile  = initFileNameFor[kernelDir];
   If[FileExistsQ[initFile],
    moveFile[initFile, newInitFile],
    (* else *)
    saveCode[newInitFile, makeCodeForInitFile[projectName]]
    ]];           
         
         
       
ClearAll[buildProjectLayout];
buildProjectLayout[tempLayoutDir_String?DirectoryQ, 
   prdir_String?DirectoryQ, projectName_String] :=
  Module[{initFile, targetProjectDir },
   shortCircuitFail[
    buildProjectLayout,
    targetProjectDir = FileNameJoin[{ tempLayoutDir , projectName}],
    CopyDirectory[prdir, targetProjectDir],
    initFile = makeInitFile[targetProjectDir, projectName],
    targetProjectDir
    ]];  
    
    
    
(* This explicitly assumes that the unzipped project was a in a \
single folder *)
ClearAll[getProjectDirNameFromUnzipped];
getProjectDirNameFromUnzipped[unzippedDir_String?DirectoryQ] :=
  With[{files = FileNames["*", unzippedDir]},
   First[files] /; MatchQ[files, {_?DirectoryQ}]
   ];
getProjectDirNameFromUnzipped[___] := $Failed;



ClearAll[ReverseIfFails];
SetAttributes[ReverseIfFails, HoldFirst];
ReverseIfFails[initCode_, mainFunc_, reverseFunc_] :=
  Module[{init, result},
   If[(init = initCode) === $Failed, Return[$Failed]];
   CleanUp[
    result = mainFunc[init],
    If[result === $Failed, reverseFunc[init]]
    ]];   
    
    
    
ClearAll[extractZippedProject];
extractZippedProject[path_, tempDir_] :=
  ReverseIfFails[
   extractArchive[path, tempDir, "ZIP"],
   getProjectDirNameFromUnzipped,
   DeleteDirectory[#, DeleteContents -> True] &
   ]; 
   
   
   
ClearAll[withTemporaryDirectory];
Options[withTemporaryDirectory] = {
   FailIfExists -> True,
   FailIfCanNotDelete -> False,
   CreateIfDoesNotExist -> True
   };
withTemporaryDirectory::nodel = 
  "Warning: unable to delete temporary directory `1`";
withTemporaryDirectory::nocreate = 
  "Unable to create temporary directory `1`";
withTemporaryDirectory::exists = "The directory `1` already exists";
SetAttributes[withTemporaryDirectory, HoldRest];
withTemporaryDirectory[tempDir_String, code_, 
   opts : OptionsPattern[]] :=
  Module[{deleted, result},
   If[DirectoryQ[tempDir],
    If[TrueQ[OptionValue[FailIfExists]],
     Message[withTemporaryDirectory::exists, tempDir];
     Throw[$Failed, error[withTemporaryDirectory]]
     ],
    (* else *)
    If[TrueQ[OptionValue[CreateIfDoesNotExist]] && 
      Quiet@CreateDirectory[tempDir] === $Failed,
     Message[withTemporaryDirectory::nocreate, tempDir];
     Throw[$Failed, error[withTemporaryDirectory]]
     ]];
   result = 
    CleanUp[
     code,
     deleted = Quiet@DeleteDirectory[tempDir, DeleteContents -> True]
     ];
   If[deleted === $Failed,
    Message[withTemporaryDirectory::nodel, tempDir];
    If[TrueQ[OptionValue[FailIfCanNotDelet]],
     Throw[$Failed, error[withTemporaryDirectory]]
     ]];
   result]; 
   
   
   
(* Assume one .tar file in the archive *)
ClearAll[unGZipProject];
unGZipProject[gzipPath_, tempDir_] :=
  Module[{ungzipped, files},
   shortCircuitFail[
    unGZipProject,
    ungzipped = extractArchive[gzipPath, tempDir, "GZIP"],
    check[Length[files = FileNames["*.tar", {ungzipped}]] == 1],
    First@files
    ]];
    
    
    
(* Assume one directory in the .tar archive. There can also be files, \
they will be ignored *)
ClearAll[unTarProject];
unTarProject[tarPath_, tempDir_] :=
  Module[{untarred, dirs},
   shortCircuitFail[
    unTarProject,
    untarred = extractArchive[tarPath, tempDir, "TAR"],
    check[
     Length[dirs = Select[FileNames["*", {untarred}], DirectoryQ]] == 
      1],
    First@dirs
    ]];
    
    
    
Clear[extractTarGzippedProject];
extractTarGzippedProject[path_String?FileExistsQ, 
   tempBaseDir_String] :=
  Module[{tempTarDir},
   tempTarDir = makeTemporaryDirectoryName[tempBaseDir];
   withTemporaryDirectory[
    tempTarDir,
    unTarProject[
     unGZipProject[path, tempTarDir],
     makeTemporaryDirectoryName[tempBaseDir]
     ],
    CreateIfDoesNotExist -> False
    ]];   
                   


ClearAll[autoSet];
SetAttributes[autoSet, HoldAll];
autoSet[var_, value_] := If[var === Automatic, var = value];



ClearAll[getProjectType];
getProjectType[path_String] :=
  With[{type = 
     Switch[ToLowerCase@FileExtension[path],
      "zip",
      	"ZIP",
      "gz",
      	If[ToLowerCase@FileExtension[FileBaseName[path]] === "tar",
       		"TARGZ",
       		$Failed
       	],
      _,
      	$Failed]},
   type];
getProjectType[___] := Throw[$Failed, error[getProjectType]];



ClearAll[javaIterate];
SetAttributes[javaIterate, HoldAll];
javaIterate[var_Symbol, code_][iteratorInstance_?JavaObjectQ] :=
  While[iteratorInstance@hasNext[], 
   With @@ Hold[{var = iteratorInstance@next[]}, code]];
javaIterate[___][___] := Throw[$Failed, error[javaIterate]];



ClearAll[save, collect];
save = Sow;
SetAttributes[collect, HoldAll];
collect[code_] := If[# === {}, #, First@#] &@Reap[code][[2]];



ClearAll[getHTTPHeaderFields];
getHTTPHeaderFields[urlConnection_?JavaObjectQ] :=
  Module[{entry},
   JavaBlock[
    With[{iter = urlConnection@getHeaderFields[]@keySet[]@iterator[]},
     If[# =!= {}, Rest[#], #] &@
      collect@javaIterate[entry, save[entry]][iter]]]];
getHTTPHeaderFields[___] := 
  Throw[$Failed, error[getHTTPHeaderFields]];
  
  
  
Clear[getAttachementFileName];
getAttachementFileName[urlString_String] :=
  JavaBlock[
   Module[{url, uconn, result},
    url = JavaNew["java.net.URL", urlString];
    uconn = url@openConnection[];
    uconn @setFollowRedirects[True];
    CleanUp[
     uconn@connect[];
     getAttachementFileName[uconn],
     uconn@disconnect[]
     ]]];

getAttachementFileName[urlConnection_?JavaObjectQ] :=
  Module[{headerFields, content, filename},
   headerFields = getHTTPHeaderFields[urlConnection];
   If[MemberQ[headerFields, "Status"] && 
     ! StringFreeQ[urlConnection@getHeaderField["Status"], "404"], 
    Throw[$Failed, error[getAttachementFileName, "404"]]
    ];
   If[MemberQ[headerFields, "Content-Disposition"] ,
    content = urlConnection@getHeaderField["Content-Disposition"];
    If[StringQ[content] && ! StringFreeQ[content, "filename=" ~~ __],
     filename = 
      StringCases[
       content, 
       "filename=" ~~ x__ ~~ (Whitespace | "") :> destringify[x]
       ]]];
   If[! ValueQ[filename] || Length[filename] =!= 1, 
    Throw[$Failed, error[getAttachementFileName, "unable_to_guess"]]
    ];
   First@filename
   ];
getAttachementFileName[___] := 
  Throw[$Failed, error[getAttachementFileName]];  



ClearAll[projectInstall];
Options[projectInstall] = {
   DestinationDirectory :> $destinationDirectory,
   TempBaseDirectory :> Automatic,
   "ProjectJarLocations" :> $projectJarLocations
   };
   
 projectInstall[URL[url_String], opts : OptionsPattern[]] :=
  Module[{type = getProjectType[url], file},
   If[type === $Failed, 
    type = getProjectType[getAttachementFileName[url]]];
   If[type === $Failed, 
    Throw[$Failed, 
     error[projectInstall, "can_not_determine_remote_type"]]
    ];
   file = Quiet@FetchURL[url, FileFilters -> {}];
   If[file  === $Failed, 
    Throw[$Failed, error[projectInstall, "download_failure"]]
    ];
   If[getProjectType[file] === $Failed,
    file = 
     Switch[type,
      "ZIP",
      	RenameFile[file, file <> ".zip"],
      "TARGZ",
      	RenameFile[file, file <> ".tar.gz"],
      _,
      	Throw[$Failed, error[projectInstall, "unknown_type"]]
      ]];
   projectInstall[file, opts]];


projectInstall[path_String?FileExistsQ, opts : OptionsPattern[]] :=
  projectInstall[path, getProjectType[path], opts];


projectInstall[path_String?FileExistsQ, type : ("ZIP" | "TARGZ"), 
   opts : OptionsPattern[]] :=
  Module[{$destinationDirectory = OptionValue[DestinationDirectory ],
    tempBaseDir = OptionValue[TempBaseDirectory],
    $tempDir, $tempLayoutDir, prdir, projectName, targetProjectDir,
    initFile, deployed, deleted},
   autoSet[tempBaseDir, 
    ApplicationDataUserDirectory["LoadedProjects"]];
   shortCircuitFail[
    projectInstall,
    $tempDir = makeTemporaryDirectoryName[tempBaseDir],
    $tempLayoutDir = makeTemporaryDirectoryName[tempBaseDir],
    (* AddToClassPath@@OptionValue["ProjectJarLocations"], *)
    prdir = 
     If[type === "ZIP",
      extractZippedProject[path, $tempDir],
      (* else *)
      extractTarGzippedProject[path, tempBaseDir]
      ],
    projectName = determineProjectName[prdir],
    check[! projectAlreadyExistsQ[$destinationDirectory, projectName]],
    withTemporaryDirectory[
     $tempLayoutDir,
     shortCircuitFail[
      projectInstall,
      targetProjectDir = 
       buildProjectLayout[$tempLayoutDir, prdir, projectName],
      deployed = 
       CopyDirectory[targetProjectDir, 
        FileNameJoin[{$destinationDirectory, projectName}]]
      ]
     ],
    deployed 
    ]];  


End[]

EndPackage[]

