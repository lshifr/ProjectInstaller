(* Mathematica Package *)

(* Created by the Wolfram Workbench 21.11.2012 *)



(* :Title: ProjectInstaller *)

(* :Context: ProjectInstaller` *)

(* :Author:
        Leonid Shifrin
        lshifr@gmail.com
*)

(* :Package Version: 1.0 *)

(* :Mathematica Version: 8.0 *)

(* :Copyright: Leonid Shifrin, 2012.  *)

(* :Discussion:

   ProjectInstaller is a simple web installer for Mathematica projects, stored
   on the web in .zip or .tar.gz formats. It attempts to download and install
   a given project into a specified project directory.   
      
*)

(* :Keywords: projects, install *)




BeginPackage["ProjectInstaller`",{"Utilities`URLTools`"}]
(* Exported symbols added here with SymbolName::usage *)

ProjectInstall::usage = 
"ProjectInstall[url, opts ] attempts to download and install the Mathematica project by following the \
url. The url must have the form URL[_String]. The project must exist on the server as an archive file \
with .zip or .tar.gz extension. 

ProjectInstall[path, opts ] ettempts to install the Mathematica project located at the path on the \
local machine. The project must be an archve file with .zip or .tar.gz extension."; 


ProjectUninstall::usage = 
"ProjectInstall[name, opts] uninstalls the project with the name name";


DestinationDirectory::usage = "An option for ProjectInstall and ProjectUninstall, that specifies a \ 
path to a local project repository. The default for that is $UserBaseDirectory/Applications";

TempBaseDirectory::usage = "An option for ProjectInstall, that specifies the location of the \
temporary directory, to be used by intermediate steps of ProjectInstall. The directory must exist
and be writable.";





Begin["`Private`"]
(* Implementation of the package *)

Needs["JLink`"];
Needs["ResourceLocator`"];
Needs["PacletManager`"];


(* Todo: replace this with a Throw[$Failed] definition *)


$testMode = False;

$projectLocation = 
	With[{pos = Position[FileNameSplit[$InputFileName], "ProjectInstaller"][[If[$testMode, -2,-1],1]]},
		FileNameTake[$InputFileName,pos]
	];
	
	
$projectJarLocations = 
	If[$testMode,
		{
			FileNameJoin[{$projectLocation, "ProjectInstaller", "Java"}],
			FileNameJoin[{$projectLocation,	"javaOutput", "Build", "jar"}]
		},
		(* else *)
		{FileNameJoin[{$projectLocation,"Java"}]}
	];

   
$destinationDirectory = 
  FileNameJoin[{$UserBaseDirectory, "Applications"}];
  
  
 
 
(******************************************************************************)
(************ 			Generic helper functions			***************)
(******************************************************************************)  
  
  
ClearAll[autoSet];
SetAttributes[autoSet, HoldAll];
autoSet[var_, value_] := 
	If[var === Automatic, var = value];  
  
  
  
ClearAll[destringify];
destringify[s_String /; StringMatchQ[s, "\"" ~~ __ ~~ "\""]] :=
	destringify[
   		StringReplace[s, LongestMatch["\"" ~~ x__ ~~ "\""] :> x]
   	];
   	
destringify[s_String] := s;  


showIt[x__]:=(Print[x];x);

  
(******************************************************************************)
(************ 			Control flow helper functions			***************)
(******************************************************************************)  

throwError[fun_, args___]:=
	Throw[$Failed, error[fun, args]];
	
	
	
ClearAll[handleError];
SetAttributes[handleError, HoldFirst];
Options[handleError] = {
   GenerateErrorMessage -> True   
};  
   
handleError[code_, attachMessageTo_Symbol, opts : OptionsPattern[]] := 
	With[{msgQ = OptionValue[GenerateErrorMessage]},
		Catch[
			code, 
			_error,
			Function[{value, tag},
				If[TrueQ@msgQ,
					Message[attachMessageTo::err, Style[First@tag, Red]]
				];
				value
     		]
     	]
     ];
     
handleError[code_]:=
	Catch[code,_error,
		 Function[{value, tag},
		 	{value, tag}
		 ]
	];
     	


ClearAll[tryAlternatives];
SetAttributes[tryAlternatives,HoldAll];
tryAlternatives[] = $Failed;
tryAlternatives[fst_, rest___]:=
	With[{res = fst},
		res /; res =!= $Failed
	];
tryAlternatives[fst_,rest___]:=
	tryAlternatives[rest];
  
  
  
SetAttributes[CleanUp, HoldAll];
CleanUp[expr_, cleanup_] :=
	Module[{exprFn, result, abort = False, rethrow = True, seq}, 
   		exprFn[] := expr;
   		result = 
    		CheckAbort[
     			Catch[
     				Catch[result = exprFn[]; rethrow = False; result],
     				 _, 
      				seq[##] &
      			], 
      			abort = True
      		];
   		cleanup;
   		If[abort, Abort[]];
   		If[rethrow, Throw[result /. seq -> Sequence]];
   		result];
   
   
   
ClearAll[parseInContext];
SetAttributes[parseInContext, HoldFirst];
Options[parseInContext] =
  	{		
   		"LocalizingContext" -> "MyLocalizingContext`",
   		"DefaultImportedContexts" :> {},
   		"ExtraImportedContexts" :> {}
   	};
parseInContext[code_, opts : OptionsPattern[]] :=
  	Module[
   	{		
    		result,
    		context = OptionValue["LocalizingContext"],
    		defcontexts = OptionValue["DefaultImportedContexts"],
    		extraContexts = OptionValue["ExtraImportedContexts" ],
    		allContexts
    	},		
   		allContexts = {Sequence @@ defcontexts, 
     Sequence @@ extraContexts};		
   		Block[{$ContextPath},			
    			CleanUp[				
     				BeginPackage[context]; Needs /@ allContexts; result = code,
     				EndPackage[]
     			];
    			result
    		]
   	];   
   
      
(* TODO: decide whether throwing an exception is indeed appropriate \
here *)
ClearAll[shortCircuitFail];
SetAttributes[shortCircuitFail, HoldRest];
shortCircuitFail[funName_Symbol, code_, rest___] :=
	With[{res = code},
   		If[res === $Failed,
    		throwError[
    			funName,     			
      			Hold[code] /. var_Symbol :> With[{result = var}, result /; True]      			
      		], 
    		(* else *)
    		If[Length[Hold[rest]] == 0, 
    			res, 
    			shortCircuitFail[funName, rest]
    		]
    	]];
    
    
    
ClearAll[ReverseIfFails];
SetAttributes[ReverseIfFails, HoldFirst];
ReverseIfFails[initCode_, mainFunc_, reverseFunc_] :=
	Module[{init, result},
   		If[(init = initCode) === $Failed, Return[$Failed]];
   		CleanUp[
    		result = mainFunc[init],
    		If[result === $Failed, reverseFunc[init]]
    	]
    ];   
        
    
    
ClearAll[check];
check[cond_] := If[! TrueQ[cond], $Failed];



ClearAll[exitOnFailure];
exitOnFailure[val_, fun_] :=
  If[val === $Failed, throwError[fun]];
  


ClearAll[javaIterate];
SetAttributes[javaIterate, HoldAll];
javaIterate[var_Symbol, code_][iteratorInstance_?JavaObjectQ] :=
	While[iteratorInstance@hasNext[], 
   		With @@ Hold[{var = iteratorInstance@next[]}, code]
   	];
   	
javaIterate[___][___] := throwError[javaIterate];



ClearAll[save, collect];

save = Sow;

SetAttributes[collect, HoldAll];
collect[code_] := 
	If[# === {}, #, First@#] &@Reap[code][[2]];



(******************************************************************************)
(************ 	Generic code generation helper functions		***************)
(******************************************************************************)


Clear[formatCode];
formatCode[code_Hold] := 
	StringReplace[
   		Function[Null, ToString[Unevaluated[#], InputForm], HoldAll] @@ code, 
   		";" :> ";\n"
   	];
    
    
(* TODO: add cleanup guarantee *)    
Clear[saveCode];
saveCode[file_, generatedCode_] :=
	With[{result = BinaryWrite[file, formatCode@generatedCode]},
   		Close[file];
   		result
   	];
   
   
   
ClearAll[makeContext];
makeContext[name_String, contextSeparator_String: "`"] := 
	name <> contextSeparator; 



(******************************************************************************)
(************ 		Files / directories helper functions		***************)
(******************************************************************************)


join = FileNameJoin[{##}]&;

in[elem_, list_List] := MemberQ[list,elem];

subdirs[dir_]:= Select[FileNames["*", {dir}], DirectoryQ]

ext[path_]:= ToLowerCase@FileExtension[path];

rmdir[dir_]:= DeleteDirectory[dir, DeleteContents -> True];



ClearAll[moveFile];
moveFile[src_String?FileExistsQ, dest_String] :=
	shortCircuitFail[moveFile,
   		CopyFile[src, dest],
   		DeleteFile[src],
   		dest
   	];
moveFile[___] := throwError[moveFile];



ClearAll[makeTemporaryDirectoryName];
makeTemporaryDirectoryName::maxit = 
  "Unable to produce a unique directory name in `1` iterations";
  
Options[makeTemporaryDirectoryName] = {
   MaxIterations -> 1000
};
   
makeTemporaryDirectoryName[globalTempDir_] :=
	Module[{tempDir, n = 0, lim,randomName},
		lim = OptionValue[MaxIterations];
 		randomName := StringJoin["temp", ToString[RandomInteger[10^5]]];
  		While[
   			FileExistsQ[
     			tempDir = globalTempDir ~ join ~ randomName
   			]     
       		&& 
       		++n < lim
       	];
  		If[n > lim,
   			Message[makeTemporaryDirectoryName::maxit, lim];
   			throwError[makeTemporaryDirectoryName]
   		];
  		tempDir]
       


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
withTemporaryDirectory[tempDir_String, code_, opts : OptionsPattern[]] :=
	Module[{deleted, result},
		If[DirectoryQ[tempDir],
    		If[TrueQ[OptionValue[FailIfExists]],
     			Message[withTemporaryDirectory::exists, tempDir];
     			throwError[withTemporaryDirectory]
     		],
    		(* else *)
    		If[TrueQ[OptionValue[CreateIfDoesNotExist]] 
    			&& Quiet@CreateDirectory[tempDir] === $Failed,
     				Message[withTemporaryDirectory::nocreate, tempDir];
     				throwError[withTemporaryDirectory]
     		]
     	];
   		result = 
    		CleanUp[code, deleted = Quiet@rmdir[tempDir]];
   		If[deleted === $Failed,
    		Message[withTemporaryDirectory::nodel, tempDir];
    		If[TrueQ[OptionValue[FailIfCanNotDelet]],
     			throwError[withTemporaryDirectory]
     		]
     	];
   		result]; 
   


(******************************************************************************)
(************ 		Archive extraction  helper functions		***************)
(******************************************************************************)

ClearAll[isJar];
isJar[s_String]:=
	FileExistsQ[s] && ToLowerCase[FileExtension[s]] === "jar";
isJar[___] := False;


ClearAll[isValidJarLocation]
isValidJarLocation[s_String?DirectoryQ] := True;
isValidJarLocation[s_String?isJar]:= True;
isValidJarLocation[___] := False;


ClearAll[verifyJarLocations];
verifyJarLocations[{__String?isValidJarLocation}]:=Null;
verifyJarLocations[___]:= 
	throwError[verifyJarLocations];


ClearAll[addProjectJarsToClassPath];
addProjectJarsToClassPath[jarLocations_]:= 
	Module[{result},
		verifyJarLocations[jarLocations];
		result = AddToClassPath @@ jarLocations;
		result /; result =!= $Failed
	];
addProjectJarsToClassPath[___]:=
	throwError[addProjectJarsToClassPath];
	
	

(* TODO Add error messages / analysis, based on Java error info *)
ClearAll[extractArchive];
(* extractArchive::nojars = "Can not find the project's jars"; *)

extractArchive[source_, dest_, type_: "ZIP"] :=
	Block[{unZip, unGzip, unTar},
   		Module[{javaError, javaExtractor, jarlocations},
    	(* Note: a global variable *)
    	jarlocations = $projectJarLocations;
    	addProjectJarsToClassPath[jarlocations];    	
    	JavaBlock[
     		javaExtractor = Quiet@JavaNew["org.lshifr.arch.ZipTarExtractor"];
     		If[javaExtractor === $Failed,
      			throwError[extractArchive, "can_not_create_java_class"];
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
         					throwError[extractArchive,"uknown_extract_method"]
         			]
       			},
      			javaError = javaExtractor@method[source, dest]
      		];
     		If[javaError @ msg =!= "Success",
      			throwError[extractArchive, {"javaError",javaError@msg}],
      			(* else *)
       			dest
      		]
     	]]
     ];




(******************************************************************************)
(************ 	Web (HTTP requests etc.)  helper functions		***************)
(******************************************************************************)


ClearAll[getHTTPHeaderFields];
getHTTPHeaderFields[urlConnection_?JavaObjectQ] :=
	Module[{entry},
   		JavaBlock[
    		With[{iter = urlConnection@getHeaderFields[]@keySet[]@iterator[]},
     			If[# =!= {}, Rest[#], #] &@ collect @
     				javaIterate[entry, save[entry]][iter]
     		]
     	]
     ];
      
getHTTPHeaderFields[___] := 
	throwError[getHTTPHeaderFields];
  
  
  
Clear[getAttachementFileName];
getAttachementFileName[urlString_String] :=
	JavaBlock[
   		Module[{url, uconn},
    		url = JavaNew["java.net.URL", urlString];
    		uconn = url@openConnection[];
    		uconn @ setFollowRedirects[True];
    		CleanUp[
     			uconn@connect[];
     			getAttachementFileName[uconn],
     			uconn@disconnect[]
     		]
     	]
    ];

getAttachementFileName[urlConnection_?JavaObjectQ] :=
	Module[{headerFields, content, filename},
   		headerFields = getHTTPHeaderFields[urlConnection];
   		If[MemberQ[headerFields, "Status"] && 
     		! StringFreeQ[urlConnection@getHeaderField["Status"], "404"], 
    			throwError[getAttachementFileName, "404"]
    	];
   		If[MemberQ[headerFields, "Content-Disposition"] ,
    		content = urlConnection@getHeaderField["Content-Disposition"];
    		If[StringQ[content] && ! StringFreeQ[content, "filename=" ~~ __],
     			filename = 
      				StringCases[
       					content, 
       					"filename=" ~~ x__ ~~ (Whitespace | "") :> destringify[x]
       				]
       		]
       	];
   		If[! ValueQ[filename] || Length[filename] =!= 1, 
    		throwError[getAttachementFileName, "unable_to_guess_project_type"]
    	];
   		First@filename
   	];
   	
getAttachementFileName[___] := 
	throwError[getAttachementFileName];  



(******************************************************************************)
(******************************************************************************)
(************ 					MAIN FUNCTIONS					***************)
(******************************************************************************)
(******************************************************************************)



(******************************************************************************)
(************ 	Determine project's properties / structure		***************)
(******************************************************************************)


ClearAll[getProjectFile];
getProjectFile[file_String?FileExistsQ] :=
	With[{strconts = Quiet@Import[file, "String"]},
   		If[! SyntaxQ[strconts ], 
    		throwError[getProjectFile, "malformed_project_file"]
    	];
   		ReleaseHold[
     		DeleteCases[
      			ToExpression[strconts, InputForm, Hold],
      			Null]
      	] /; strconts =!= $Failed
   ];
   
getProjectFile[file_String?FileExistsQ] :=
	throwError[getProjectFile, "unimportable_project_file"];

getProjectFile[___] :=
	throwError[getProjectFile];
	
	
	
ClearAll[verifyProjectFileContent];
verifyProjectFileContent[content_]:=
	With[{result = 	Quiet@ExportString[content, "JSON"]},
		content /; result =!= $Failed && MatchQ[content, {__Rule}| _Rule]
	];
	
verifyProjectFileContent[___]:=
	throwError[verifyProjectFileContent];	



Clear[getProjectName];
getProjectName[prfile_] :=
	verifyProjectFileContent@getProjectFile[prfile] /. 
  		{rules__} :> 
  			("name" /. {rules} /. "name" :> 
  					throwError[getProjectName]);
     
    
ClearAll[randomContext];
randomContext[]:=
	"MyRandomContext" <> ToString[RandomInteger[10^6]] <> "`"    
    
    
    
Clear[singlePackageInProjectQ];
singlePackageInProjectQ[prdir_] :=
	Length[FileNames["*.m", {prdir}]] == 1;
	
	
ClearAll[getStringPacletInformation];
getStringPacletInformation[file_String] :=
	With[{cont = randomContext[]},
		Module[{result},
    		result = 
    			parseInContext[
      				getProjectFile[file] //.
       					Verbatim[Rule][lhs_Symbol, rhs_] :> (ToString[lhs] -> rhs),
      				"LocalizingContext" -> cont,
      				"ExtraImportedContexts"  -> {"PacletManager`"}
      			];
    		Remove[Evaluate[cont <> "*"]];
    		result /; Head[result] === PacletManager`Paclet
    	]
    ];
    
getStringPacletInformation[file_String] :=
 throwError[getStringPacletInformation, "malformed_pacletinfo_file"]
	
getStringPacletInformation[___]:=
	throwError[getStringPacletInformation];
	  
  
ClearAll[getNameFromPacletInfo];
getNameFromPacletInfo[file_String?FileExistsQ]:=
	With[{name = "Name"/.PacletInformation[getStringPacletInformation[file]]},
		name /; name =!= "Name"	
	];
getNameFromPacletInfo[file_String?FileExistsQ]:= $Failed;

getNameFromPacletInfo[file_String]:=
	throwError[getNameFromPacletInfo,"file_does_not_exist"];
	
getNameFromPacletInfo[args___]:=
	throwError[getNameFromPacletInfo,{args}];	
	 
	  
  
  
  
(* TODO: split this function into project name finding proper, and the check for main package existence *)  
ClearAll[determineProjectName];
determineProjectName::nopckg = 
  "A package with the same name as the project: `1`, expected in the \
top level of the porject directory";
determineProjectName[prdir_String?DirectoryQ] :=
	Module[{prfile, packages , prname, pacletinfo, pacletname},
   		prfile = prdir ~ join ~ "project.m";
   		packages = FileNames["*.m", {prdir}];
   		pacletinfo = prdir ~ join ~ "PacletInfo.m"; 
   		prname = 
   			Which[
     			FileExistsQ[prfile],
     				getProjectName[prfile],
     			FileExistsQ[pacletinfo] && (pacletname = getNameFromPacletInfo[pacletinfo])=!=$Failed, 
     				pacletname,
     			Length[packages] == 1,
     				FileBaseName[First@packages],
     			Length[packages] == 0,
     				throwError[determineProjectName,"no_packages_on_the_top_level"],
     			True,
     				throwError[determineProjectName,"no_project_file_and_more_than_one_package"]
     		];
     	(* This check is needed since Needs will be looking for the context with the name of the project *)
     	(*
   		With[{mainPackage = prdir ~ join ~ (prname <> ".m")},
    		If[! mainPackage ~ in ~ packages,
     			Message[determineProjectName::nopckg, FileNameTake[mainPackage, -1]];
     			throwError[determineProjectName,"no_package_matching_project_name"];
     		]
     	]; *)
   		prname
   ];
   
determineProjectName[___]:=
	throwError[determineProjectName];
	
	   
   
ClearAll[projectAlreadyExistsQ];
projectAlreadyExistsQ[prdir_String?DirectoryQ, prname_String] :=
	Select[
		FileNames[{"*"}, prdir],
    	FileBaseName[#] === prname && (DirectoryQ[#] || FileExtension[#] === "m") &
    ] =!= {}; 
    
    
    
ClearAll[failIfProjectAlreadyExists];    
failIfProjectAlreadyExists[prdir_String?DirectoryQ, prname_String]:=
	If[projectAlreadyExistsQ[prdir,prname],
		throwError[failIfProjectAlreadyExists,"project_already_exists"]
	];
	
failIfProjectAlreadyExists[___]:=
	throwError[failIfProjectAlreadyExists];
	    
    
    
ClearAll[initFileNameFor];
initFileNameFor[dir_String] :=  dir ~ join ~ "init.m";
      

ClearAll[getProjectType];
getProjectType[path_String] :=
	With[{type = 
    	Switch[ext[path],
      		"zip",
      			"ZIP",
      		"gz",
      			If[ext[FileBaseName[path]] === "tar",
       				"TARGZ",
       				$Failed
       			],
      		_,
      			$Failed]
      	},
   		type];
   
getProjectType[___] := 
	throwError[getProjectType];



(******************************************************************************)
(************ 	Specializing archive extraction to the projects	***************)
(******************************************************************************) 



(* TODO: express in more generic functions *)    
(* This explicitly assumes that the unzipped project was a in a \
single folder *)
ClearAll[getProjectDirNameFromUnzipped];
getProjectDirNameFromUnzipped[unzippedDir_String?DirectoryQ] :=
	With[{files = FileNames["*", unzippedDir]},
		First[files] /; MatchQ[files, {_?DirectoryQ}]
	];
getProjectDirNameFromUnzipped[___] := $Failed;
    
    
    
(* TODO: make more generic *)    
ClearAll[extractZippedProject];
extractZippedProject[path_, tempDir_] :=
	ReverseIfFails[
		extractArchive[path, tempDir, "ZIP"],
   		getProjectDirNameFromUnzipped,
   		rmdir
   	];  
   
   
 
(* TODO: make more generic *)   
(* Assume one .tar file in the archive *)
ClearAll[unGZipProject];
unGZipProject[gzipPath_, tempDir_] :=
	Module[{ungzipped, files},
		shortCircuitFail[unGZipProject,
    		ungzipped = extractArchive[gzipPath, tempDir, "GZIP"],
    		check[Length[files = FileNames["*.tar", {ungzipped}]] == 1],
    		First@files
    	]];
    
    
(* TODO: make more generic *)    
(* Assume one directory in the .tar archive. There can also be files, \
they will be ignored *)
ClearAll[unTarProject];
unTarProject[tarPath_, tempDir_] :=
	Module[{untarred, dirs},
		shortCircuitFail[unTarProject,
    		untarred = extractArchive[tarPath, tempDir, "TAR"],
    		check[Length[dirs = subdirs[untarred]] == 1],
    		First@dirs
    	]];
    
    
    
Clear[extractTarGzippedProject];
extractTarGzippedProject[path_String?FileExistsQ, tempBaseDir_String] :=
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
                   


    
    
      
(******************************************************************************)
(************ 	Transformations / code gen / layout building	***************)
(******************************************************************************)  
  
  
ClearAll[makeCodeForInitFile];
makeCodeForInitFile[projectName_String] :=
	With[{context = # <> # &[makeContext[projectName]]},
		Hold[Get[context]] (* With necessary to inject code *)
	]
             
          
  
ClearAll[makeInitFile];
(* 
** It is assumed that by the time this function is called, all the \
** files have been copied to the targetProjectDir 
*)
makeInitFile[targetProjectDir_String?DirectoryQ, projectName_String] :=  
	Module[{kernelDir, newInitFile, initFile},
		(* If init.m already exists in the Kernel subfolder, exit *) 
		kernelDir = targetProjectDir ~ join ~ "Kernel";
		newInitFile  = initFileNameFor[kernelDir];
		If[FileExistsQ[newInitFile],Return[newInitFile]];		
		(* Top-level init.m file - not in the Kernel subfolder - if it exists *)
		initFile = initFileNameFor[targetProjectDir];		
   		exitOnFailure[
    		kernelDir = CreateDirectory[kernelDir],
    		makeInitFile
    	];   		
   		If[FileExistsQ[initFile],
    		moveFile[initFile, newInitFile],
    		(* else *)
    		saveCode[newInitFile, makeCodeForInitFile[projectName]]
    	]
    ];           
         
         
       
ClearAll[buildProjectLayout];
buildProjectLayout[tempLayoutDir_String?DirectoryQ, prdir_String?DirectoryQ, projectName_String] :=
	Module[{initFile, targetProjectDir },
		shortCircuitFail[buildProjectLayout,
    		targetProjectDir =  tempLayoutDir ~ join ~ projectName,
    		CopyDirectory[prdir, targetProjectDir],
    		initFile = makeInitFile[targetProjectDir, projectName],
    		targetProjectDir
    	]];  
			


ClearAll[projectInstall];
Options[projectInstall] = {
   DestinationDirectory :> $destinationDirectory,
   TempBaseDirectory :> Automatic,
   "ProjectJarLocations" :> $projectJarLocations
};
   
 projectInstall[URL[url_String], opts : OptionsPattern[]] :=
 	Module[{type, file},
 		InstallJava[];
 		type = 
 			tryAlternatives[
 				getProjectType[url],
 				getProjectType[getAttachementFileName[url]]
 			]; 		
   		If[type === $Failed, 
    		throwError[projectInstall, "can_not_determine_remote_type"]
    	];
   		file = Quiet@FetchURL[url, FileFilters -> {}];
   		If[file  === $Failed, 
    		throwError[projectInstall, "download_failure"]
    	];
   		If[getProjectType[file] === $Failed,
    		file = 
     			Switch[type,
      				"ZIP",
      					RenameFile[file, file <> ".zip"],
      				"TARGZ",
      					RenameFile[file, file <> ".tar.gz"],
      				_,
      					throwError[projectInstall, "unknown_type"]
      			]
      	];
   		projectInstall[file, opts]
   	];

projectInstall[path_String?FileExistsQ, opts : OptionsPattern[]] :=
	projectInstall[path, getProjectType[path], opts];

projectInstall[path_String?FileExistsQ, type : ("ZIP" | "TARGZ"),  opts : OptionsPattern[]] :=
	Module[{$destinationDirectory = OptionValue[DestinationDirectory],
    	tempBaseDir = OptionValue[TempBaseDirectory],
    	$tempDir, $tempLayoutDir, prdir, projectName, targetProjectDir,
     	deployed
     	},
   		autoSet[tempBaseDir, ApplicationDataUserDirectory["LoadedProjects"]];
   		shortCircuitFail[projectInstall,
    		$tempDir = makeTemporaryDirectoryName[tempBaseDir],
    		$tempLayoutDir = makeTemporaryDirectoryName[tempBaseDir],    		
    		prdir = 
     			If[type === "ZIP",
      				extractZippedProject[path, $tempDir],
      				(* else *)
      				extractTarGzippedProject[path, tempBaseDir]
      			],
    		projectName = determineProjectName[prdir],
    		failIfProjectAlreadyExists[$destinationDirectory, projectName],
    		withTemporaryDirectory[
     			$tempLayoutDir,
     			shortCircuitFail[projectInstall,
      				targetProjectDir =
      					buildProjectLayout[$tempLayoutDir, prdir, projectName],
      				deployed = 
       					CopyDirectory[
       						targetProjectDir, 
        					$destinationDirectory ~ join ~ projectName
        				]
      			]
     		],
    		deployed 
    	]
    ]; 
    
    
    
ClearAll[projectUninstall];
Options[projectUninstall] = {
   DestinationDirectory :> $destinationDirectory
};

projectUninstall[prname_String, opts : OptionsPattern[]] :=
	With[{prdir = OptionValue[DestinationDirectory]},
   		rmdir[prdir ~ join ~ prname] /; projectAlreadyExistsQ[prdir, prname]
 	];
 	
projectUninstall[prname_String, opts : OptionsPattern[]]:= {};
 	
 	
 	
 	
(******************************************************************************) 	
(******************************************************************************)
(************ 					PUBLIC INTERFACE				***************)
(******************************************************************************) 	
(******************************************************************************) 	
 	
 	
ClearAll[handleErrorFor]; 
 	
handleErrorFor[ProjectInstall][error[fun_,args___]]:=
	handleErrorFor[ProjectInstall][fun][args];
	
handleErrorFor[ProjectInstall][failIfProjectAlreadyExists]["project_already_exists"]:=
	Message[ProjectInstall::exists];
	
handleErrorFor[ProjectInstall][getAttachementFileName]["404"]:=
	Message[ProjectInstall::notfnd];
	
handleErrorFor[ProjectInstall][determineProjectName]["no_packages_on_the_top_level"]:=
	Message[ProjectInstall::nopckg];
	
handleErrorFor[ProjectInstall][determineProjectName]["no_project_file_and_more_than_one_package"]:=
	Message[ProjectInstall::manypckg];
	
handleErrorFor[ProjectInstall][determineProjectName]["no_package_matching_project_name"]:=
	Message[ProjectInstall::noprjpckg];
	
handleErrorFor[ProjectInstall][getProjectFile]["malformed_project_file"]:=
	Message[ProjectInstall::badprjfile];
	
handleErrorFor[ProjectInstall][verifyProjectFileContent][]:=
	Message[ProjectInstall::malfprjfile];
	
handleErrorFor[ProjectInstall][getAttachementFileName]["unable_to_guess_project_type"]:=
	Message[ProjectInstall::unknwntype];
	
handleErrorFor[ProjectInstall][f_][args___]:=
	Message[ProjectInstall::generr, Style[f,Red],Style[{args},Red]];


 	
 	
ClearAll[ProjectInstall]; 

ProjectInstall::exists = "The project already exists. Uninstall it first";

ProjectInstall::notfnd = "No project archive file found on the server";

ProjectInstall::nopckg = "The project has no packages in the top-level directory, and no project descriptor file \
(project.m) either. It is therefore impossible for the installer to determine project's name";

ProjectInstall::manypckg = "The project has no project descriptor (project.m file) and more than one package in the  \
top-level project directory. It is therefore impossible for the installer to determine project's name";

ProjectInstall::noprjpckg = "The project has no package in the top-level directory with the name mathcing the name of \
the project specified in project's descriptor file (project.m)";

ProjectInstall::badprjfile = "The project descriptor file (project.m) has syntax errors";

ProjectInstall::malfprjfile = "The project descriptor file (project.m) is malformed. It must contain a rule or a list \
of rules, possibly nested, containing strings and numbers only";

ProjectInstall::unknwntype = "Unable to guess the project's type by reading the HTTP header";

ProjectInstall::generr = "General error. The failure happened in function `1`, with the following additional information: `2`";

ProjectInstall::badargs = "Bad number and /or types of arguments. The arguments were `1`";
	
Options[ProjectInstall] = {
	DestinationDirectory :> $destinationDirectory,
   	TempBaseDirectory :> Automatic
};

ProjectInstall[url: URL[_String], opts : OptionsPattern[]]:=
	With[{result= handleError@Quiet@projectInstall[url, opts]},
		If[MatchQ[result, {$Failed, _error}],
			handleErrorFor[ProjectInstall][Last@result];
			Return[$Failed]
		];
		result		
	]; 	   
 	   
ProjectInstall[path_String?FileExistsQ, opts : OptionsPattern[]] :=   
	projectInstall[path, opts];
	
ProjectInstall[args___]:=
	(
		Message[ProjectInstall::badargs,Style[{args},Red]];
		$Failed
	);	 
	
	
	
ClearAll[ProjectUninstall];

ProjectUninstall::badargs = "Bad number and /or types of arguments. The arguments were `1`";

Options[ProjectUninstall] = {
   DestinationDirectory :> $destinationDirectory
};

ProjectUninstall[prname_String, opts : OptionsPattern[]] :=
	projectUninstall[prname, opts];		  


ProjectUninstall[args___]:=
	(
		Message[ProjectUninstall::badargs,Style[{args},Red]];
		$Failed
	);



End[]


EndPackage[]

