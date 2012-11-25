package org.lshifr.arch;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.zip.GZIPInputStream;

import org.apache.commons.compress.archivers.ArchiveException;
import org.apache.commons.compress.archivers.ArchiveStreamFactory;
import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream;
import org.apache.commons.compress.archivers.zip.ZipArchiveEntry;
import org.apache.commons.compress.archivers.zip.ZipArchiveInputStream;
import org.apache.commons.compress.utils.IOUtils;
import org.apache.log4j.Logger;

public class ZipTarExtractor {

	private static Logger LOG = Logger.getLogger("ProjectInstaller");
	private List<File> resultingFiles = new ArrayList<File>();
	public ArchiveError lastError;

	abstract class GenericExtractor {
		protected abstract List<File> extract(File inputFile, File outputDir)
				throws FileNotFoundException, IOException, ArchiveException;

		public ArchiveError extract(String inputFileName, String outputDirName) {
			ArchiveError result = ArchiveError.SUCCESS;
			File input = new File(inputFileName);
			if (!input.exists()) {
				result = ArchiveError.INPUTFILE_NOT_FOUND;
			}
			File output = new File(outputDirName);
			if (output.exists()) {
				result = ArchiveError.OUTPUTDIR_INVALID;
			}
			boolean created = false;
			try {
				created = output.mkdir();
			} catch (SecurityException ex) {
				// Nothing
			}
			if (!created) {
				result = ArchiveError.CANNOT_CREATE_DIRS;
			}
			try {
				resultingFiles = extract(input, output);
			} catch (ArchiveException ex) {
				result = ArchiveError.ARCHIVE_ERROR;
			} catch (FileNotFoundException ex) {
				result = ArchiveError.SOME_FILE_NOT_FOUND;
			} catch (IOException ex) {
				result = ArchiveError.IO_ERROR;
			}
			lastError = result;
			return result;
		}
	}

	public List<File> getResultingFiles() {
		return resultingFiles;
	}

	// TODO: close resources properly

	/**
	 * Untar an input file into an output file.
	 * 
	 * The output file is created in the output folder, having the same name as
	 * the input file, minus the '.tar' extension.
	 * 
	 * @param inputFile
	 *            the input .tar file
	 * @param outputDir
	 *            the output directory file.
	 * @throws IOException
	 * @throws FileNotFoundException
	 * 
	 * @return The {@link List} of {@link File}s with the untared content.
	 * @throws ArchiveException
	 */
	private static List<File> unTar(final File inputFile, final File outputDir)
			throws FileNotFoundException, IOException, ArchiveException {

		LOG.info(String.format("Untaring %s to dir %s.", inputFile
				.getAbsolutePath(), outputDir.getAbsolutePath()));

		final List<File> untaredFiles = new LinkedList<File>();
		final InputStream is = new FileInputStream(inputFile);
		final TarArchiveInputStream debInputStream = (TarArchiveInputStream) new ArchiveStreamFactory()
				.createArchiveInputStream("tar", is);
		TarArchiveEntry entry = null;
		while ((entry = (TarArchiveEntry) debInputStream.getNextEntry()) != null) {
			final File outputFile = new File(outputDir, entry.getName());
			if (entry.isDirectory()) {
				LOG.info(String.format(
						"Attempting to write output directory %s.", outputFile
								.getAbsolutePath()));
				if (!outputFile.exists()) {
					LOG.info(String.format(
							"Attempting to create output directory %s.",
							outputFile.getAbsolutePath()));
					if (!outputFile.mkdirs()) {
						throw new IllegalStateException(String.format(
								"Couldn't create directory %s.", outputFile
										.getAbsolutePath()));
					}
				}
			} else {
				LOG.info(String.format("Creating output file %s.", outputFile
						.getAbsolutePath()));
				File parent = outputFile.getParentFile();
				if (!parent.exists()) {
					LOG
							.info(String
									.format(
											"Got a file entry before the parent directory entry."
													+ " Attempting to create the parent directory directory %s.",
											parent.getAbsolutePath()));
					if (!parent.mkdirs()) {
						throw new IllegalStateException(String.format(
								"Couldn't create directory %s.", parent
										.getAbsolutePath()));
					}
				}
				outputFile.createNewFile();
				final OutputStream outputFileStream = new FileOutputStream(
						outputFile);
				IOUtils.copy(debInputStream, outputFileStream);
				outputFileStream.close();
			}
			untaredFiles.add(outputFile);
		}
		debInputStream.close();

		return untaredFiles;
	}

	
	//TODO: close resources properly
	
	/**
	 * Ungzip an input file into an output file.
	 * <p>
	 * The output file is created in the output folder, having the same name as
	 * the input file, minus the '.gz' extension.
	 * 
	 * @param inputFile
	 *            the input .gz file
	 * @param outputDir
	 *            the output directory file.
	 * @throws IOException
	 * @throws FileNotFoundException
	 * 
	 * @return The {@File} with the ungzipped content.
	 */
	private static File unGzip(final File inputFile, final File outputDir)
			throws FileNotFoundException, IOException {

		LOG.info(String.format("Ungzipping %s to dir %s.", inputFile
				.getAbsolutePath(), outputDir.getAbsolutePath()));

		final File outputFile = new File(outputDir, inputFile.getName()
				.substring(0, inputFile.getName().length() - 3));

		final GZIPInputStream in = new GZIPInputStream(new FileInputStream(
				inputFile));
		final FileOutputStream out = new FileOutputStream(outputFile);

		for (int c = in.read(); c != -1; c = in.read()) {
			out.write(c);
		}

		in.close();
		out.close();

		return outputFile;
	}

	// TODO: close resources properly

	/**
	 * Unzip an input file into an output file.
	 * 
	 * The output file is created in the output folder, having the same name as
	 * the input file, minus the '.zip' extension.
	 * 
	 * @param inputFile
	 *            the input .zip file
	 * @param outputDir
	 *            the output directory file.
	 * @throws IOException
	 * @throws FileNotFoundException
	 * 
	 * @return The {@link List} of {@link File}s with the untared content.
	 * @throws ArchiveException
	 */
	private static List<File> unZip(final File inputFile, final File outputDir)
			throws FileNotFoundException, IOException, ArchiveException {

		LOG.info(String.format("Unzipping %s to dir %s.", inputFile
				.getAbsolutePath(), outputDir.getAbsolutePath()));

		final List<File> unzippedFiles = new LinkedList<File>();
		final InputStream is = new FileInputStream(inputFile);
		final ZipArchiveInputStream debInputStream = (ZipArchiveInputStream) new ArchiveStreamFactory()
				.createArchiveInputStream("zip", is);
		ZipArchiveEntry entry = null;
		while ((entry = (ZipArchiveEntry) debInputStream.getNextEntry()) != null) {
			final File outputFile = new File(outputDir, entry.getName());
			if (entry.isDirectory()) {
				LOG.info(String.format(
						"Attempting to write output directory %s.", outputFile
								.getAbsolutePath()));
				if (!outputFile.exists()) {
					LOG.info(String.format(
							"Attempting to create output directory %s.",
							outputFile.getAbsolutePath()));
					if (!outputFile.mkdirs()) {
						throw new IllegalStateException(String.format(
								"Couldn't create directory %s.", outputFile
										.getAbsolutePath()));
					}
				}
			} else {
				LOG.info(String.format("Creating output file %s.", outputFile
						.getAbsolutePath()));
				File parent = outputFile.getParentFile();
				if (!parent.exists()) {
					LOG
							.info(String
									.format(
											"Got a file entry before the parent directory entry."
													+ " Attempting to create the parent directory directory %s.",
											parent.getAbsolutePath()));
					if (!parent.mkdirs()) {
						throw new IllegalStateException(String.format(
								"Couldn't create directory %s.", parent
										.getAbsolutePath()));
					}
				}
				outputFile.createNewFile();
				final OutputStream outputFileStream = new FileOutputStream(
						outputFile);
				IOUtils.copy(debInputStream, outputFileStream);
				outputFileStream.close();
			}
			unzippedFiles.add(outputFile);
		}
		debInputStream.close();
		return unzippedFiles;
	}

	public ArchiveError unTar(String inputFileName, String outputDirName) {
		return new GenericExtractor() {
			@Override
			protected List<File> extract(File inputFile, File outputDir)
					throws FileNotFoundException, IOException, ArchiveException {
				return unTar(inputFile, outputDir);
			}
		}.extract(inputFileName, outputDirName);
	}

	public ArchiveError unGzip(String inputFileName, String outputDirName) {
		return new GenericExtractor() {
			@Override
			protected List<File> extract(File inputFile, File outputDir)
					throws FileNotFoundException, IOException, ArchiveException {
				List<File> result = new ArrayList<File>();
				result.add(unGzip(inputFile, outputDir));
				return result;
			}
		}.extract(inputFileName, outputDirName);
	}

	public ArchiveError unZip(String inputFileName, String outputDirName) {
		return new GenericExtractor() {
			@Override
			protected List<File> extract(File inputFile, File outputDir)
					throws FileNotFoundException, IOException, ArchiveException {
				return unZip(inputFile, outputDir);
			}
		}.extract(inputFileName, outputDirName);
	}

}
