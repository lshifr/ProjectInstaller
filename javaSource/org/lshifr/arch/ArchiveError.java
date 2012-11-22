package org.lshifr.arch;

public enum ArchiveError {
	SUCCESS("Success"),
	INPUTFILE_NOT_FOUND("Input_file_not_found"),
	SOME_FILE_NOT_FOUND("Some_file_not_found"),
	OUTPUTDIR_INVALID("output_dir_invalid"),
	CANNOT_CREATE_DIRS("cannot_create_dirs"),
	ARCHIVE_ERROR("archive_error"),
	IO_ERROR("io_error");
	
	public final String msg;
	
	ArchiveError(String msg){
		this.msg= msg;
	}	
}
