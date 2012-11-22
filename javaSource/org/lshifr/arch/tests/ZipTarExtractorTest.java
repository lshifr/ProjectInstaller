package org.lshifr.arch.tests;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

import org.apache.commons.compress.archivers.ArchiveException;
import org.lshifr.arch.ArchiveError;
import org.lshifr.arch.ZipTarExtractor;

abstract class SimpleConsoleTester{		
	
	protected abstract ArchiveError extract(String inputFileName, String outputDirName,ZipTarExtractor extr );		
	
	public void uncompress(String inputFileName, String outputDirName, ZipTarExtractor extr){
		ArchiveError res = extract(inputFileName,outputDirName,extr);
		if(res != ArchiveError.SUCCESS){
			System.out.println(res.msg);
		}else{
			for(File file:extr.getResultingFiles()){
				System.out.println(file.getName());
			}
		}
	}
}

public class ZipTarExtractorTest {
	
	

	
	private static void testUnzip(String inputFileName, String outputDirName){
		ZipTarExtractor extr = new ZipTarExtractor();
		new SimpleConsoleTester(){
			@Override
			protected ArchiveError extract(String inputFileName,
					String outputDirName, ZipTarExtractor extr) {
				return extr.unZip(inputFileName, outputDirName);
			}}.uncompress(inputFileName, outputDirName, extr);
	}
	
	
	private static void testUntar(String inputFileName, String outputDirName){
		ZipTarExtractor extr = new ZipTarExtractor();
		new SimpleConsoleTester(){
			@Override
			protected ArchiveError extract(String inputFileName,
					String outputDirName, ZipTarExtractor extr) {
				return extr.unTar(inputFileName, outputDirName);
			}}.uncompress(inputFileName, outputDirName, extr);
	}
	
	
	
	private static void testUngzip(String inputFileName, String outputDirName){
		ZipTarExtractor extr = new ZipTarExtractor();
		new SimpleConsoleTester(){
			@Override
			protected ArchiveError extract(String inputFileName,
					String outputDirName, ZipTarExtractor extr) {
				return extr.unGzip(inputFileName, outputDirName);
			}}.uncompress(inputFileName, outputDirName, extr);
	}
	
	
	
	public static void main(String[] args) throws FileNotFoundException, IOException, ArchiveException{
		
		
		testUntar("C:/Users/Archie/Documents/gist4097351.tar","C:/Users/Archie/Documents/gistTest");
		testUngzip("C:/Users/Archie/Documents/gist4097351.tar.gz","C:/Users/Archie/Documents/gistTest.tar");
		testUnzip("C:/Temp/Loader.zip","C:/Users/Archie/Documents/Loader");
		
		/*
		
		File input = new File("C:/Users/Archie/Documents/gist4097351.tar");
		File output = new File("C:/Users/Archie/Documents/gistTest");
		output.mkdir();
		List<File> files = unTar(input, output);
		System.out.println("Testing untar");
		for(File file:files){
			System.out.println(file.getName());
		}
		
		File inputZip = new File("C:/Users/Archie/AppData/Local/Temp/Temp48592___master.zip");
		File outputZip = new File("C:/Users/Archie/Documents/Formatter");
		if(!outputZip.mkdir()){
			System.out.println("Could not create the directory");
			return;
		}
		System.out.println("Testing unzip");
		files = unZip(inputZip, outputZip);
		for(File file:files){
			System.out.println(file.getName());
		}
		
		inputZip = new File("C:/Temp/Loader.zip");
		outputZip = new File("C:/Users/Archie/Documents/Loader");
		if(!outputZip.mkdir()){
			System.out.println("Could not create the directory");
			return;
		}
		System.out.println("Testing unzip");
		files = unZip(inputZip, outputZip);
		for(File file:files){
			System.out.println(file.getName());
		}
		
		*/
	}
	
}
