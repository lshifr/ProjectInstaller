(* Mathematica package *)



Needs["Utilities`URLTools`"];

 Module[{ dir, files, root, result},
  dir = FileNameJoin[{$UserBaseDirectory, "Applications","ProjectInstaller"}];
  If[DirectoryQ[dir], DeleteDirectory[dir, DeleteContents -> True]];
  files = 
  	ExtractArchive[
  	 FetchURL[
     	"https://github.com/lshifr/ProjectInstaller/raw/master/Release/ProjectInstaller.zip"
     ]];
  root = Cases[files, r_ /; StringMatchQ[r, __ ~~ "ProjectInstaller"]];
  If[Length[root] =!= 1, result = $Failed, root = First@root];
  If[result === $Failed,
  	$Failed, 
  	(* else *) 
  	dir = CopyDirectory[root, dir];
  	DeleteDirectory[root, DeleteContents -> True];
  	dir
  ]
 ]
  

